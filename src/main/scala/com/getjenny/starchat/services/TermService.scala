package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 10/03/17.
  */

import java.io.File
import java.net.URL

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.analyzer.util.VectorUtils
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.analyzer.utils.TextToVectorsTools
import com.getjenny.starchat.entities._
import com.getjenny.starchat.services.esclient.TermElasticClient
import com.getjenny.starchat.utils.Index
import org.elasticsearch.action.admin.indices.analyze.{AnalyzeRequest, AnalyzeResponse}
import org.elasticsearch.action.bulk._
import org.elasticsearch.action.get.{GetResponse, MultiGetItemResponse, MultiGetRequest, MultiGetResponse}
import org.elasticsearch.action.index.IndexRequest
import org.elasticsearch.action.search.{SearchRequest, SearchResponse, SearchScrollRequest, SearchType}
import org.elasticsearch.action.update.UpdateRequest
import org.elasticsearch.client.{RequestOptions, RestHighLevelClient}
import org.elasticsearch.common.unit.TimeValue
import org.elasticsearch.common.xcontent.XContentBuilder
import org.elasticsearch.common.xcontent.XContentFactory._
import org.elasticsearch.index.query.{BoolQueryBuilder, QueryBuilders}
import org.elasticsearch.rest.RestStatus
import org.elasticsearch.search.builder.SearchSourceBuilder
import scalaz.Scalaz._

import scala.collection.JavaConverters._
import scala.collection.immutable.{List, Map}
import scala.concurrent.Future

case class TermServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
  * Implements functions, eventually used by TermResource
  */
object TermService extends AbstractDataService {
  override val elasticClient: TermElasticClient.type = TermElasticClient
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  val defaultOrg = TermElasticClient.commonIndexDefaultOrgPattern

  /** fetch the index arbitrary pattern
    *
    * @return the index arbitrary pattern
    */
  def commonIndexArbitraryPattern: String = elasticClient.commonIndexArbitraryPattern

  /** transform a vector of numerical values to a string payload which can be stored on Elasticsearch
    *
    * @param vector a vector of values
    * @tparam T the type of the Vector
    * @return a string with the payload <value index>|<value>
    */
  private[this] def payloadVectorToString[T](vector: Vector[T]): String = {
    vector.zipWithIndex.map{case(term, index) => index.toString + "|" + term.toString}.mkString(" ")
  }

  /** transform a Key,Value map to a string payload which can be stored on Elasticsearch
    *
    * @param payload the map of values
    * @tparam T the type of the key
    * @tparam U the type of the value
    * @return a string with the payload <value index>|<value>
    */
  private[this] def payloadMapToString[T, U](payload: Map[T, U]): String = {
    payload.map{case(e1, e2) => e1.toString + "|" + e2.toString}.mkString(" ")
  }

  /** transform a payload string (non sparse vector) to a Double vector
    *
    * @param payload the payload <value index>|<value>
    * @return a double vector
    */
  private[this] def payloadStringToDoubleVector(payload: String): Vector[Double] = {
    val vector: Vector[Double] = payload.split(" ").map(x => {
      val termTuple: Double = x.split("\\|") match {
        case Array(_, value) => value.toDouble
        case _ =>
          throw TermServiceException("unable to convert payload string to double vector")
      }
      termTuple
    }).toVector
    vector
  }

  /** transform a payload string (non sparse vector) to a key, value Map[String, Double]
    *
    * @param payload the payload <value index>|<value>
    * @return a key, value map
    */
  private[this] def payloadStringToMapStringDouble(payload: String): Map[String, Double] = {
    val m: Map[String, Double] = payload.split(" ").map(x => {
      val termTuple = x.split("\\|") match {
        case Array(key, value) => (key, value.toDouble)
        case _ =>
          throw TermServiceException("unable to convert string to string->double map")
      }
      termTuple
    }).toMap
    m
  }

  /** transform a payload string (non sparse vector) to a key, double value Map[Int, Double]
    *
    * @param payload the payload <value index>|<value>
    * @return a key, value map
    */
  private[this] def payloadStringToMapIntDouble(payload: String): Map[Int, Double] = {
    payload.split(" ").map(x => {
      val termTuple = x.split("\\|") match {
        case Array(index, value) => (index.toInt, value.toDouble)
        case _ =>
          throw TermServiceException("unable to convert string to int->double map")
      }
      termTuple
    }).toMap
  }

  /** transform a payload string (non sparse vector) to a key, value Map[String, String]
    *
    * @param payload the payload <value index>|<value>
    * @return a key, value map
    */
  private[this] def payloadStringToMapStringString(payload: String): Map[String, String] = {
    payload.split(" ").map(x => {
      val termTuple = x.split("\\|") match {
        case Array(key, value) => (key, value)
        case _ =>
          throw TermServiceException("unable to convert string to string->string map")
      }
      termTuple
    }).toMap
  }

  /** Populate synonyms from resource file (a default synonyms list)
    *
    * @param indexName name of the index
    * @param refresh whether to call an index update on ElasticSearch or not
    * @return a return message with the number of successfully and failed indexing operations
    */
  def indexDefaultSynonyms(indexName: String,
                           refresh: Int = 0) : Future[UpdateDocumentsResult] = {
    val (_, language, _) = Index.patternsFromIndexName(indexName)
    val synonymsPath: String = "/index_management/json_index_spec/" + language + "/synonyms.csv"
    val synonymsResource: URL = getClass.getResource(synonymsPath)
    val synFile = new File(synonymsResource.toString.replaceFirst("file:", ""))
    this.indexSynonymsFromCsvFile(indexName = indexName, file = synFile)
  }

  /** upload a file with Synonyms, it replace existing terms but does not remove synonyms for terms not in file.
    *
    * @param indexName the index name
    * @param file a File object with the synonyms
    * @param skipLines how many lines to skip
    * @param separator a separator, usually the comma character
    * @return the IndexDocumentListResult with the indexing result
    */
  def indexSynonymsFromCsvFile(indexName: String, file: File, skipLines: Int = 0, separator: Char = ','):
  Future[UpdateDocumentsResult] = Future {
    val documents = FileToDocuments.getTermsDocumentsFromCSV(log = log,
      file = file, skipLines = skipLines, separator = separator).toList
    updateTerm(indexName, Terms(terms = documents), 0)
  }

  /** index terms on Elasticsearch
    *
    * @param indexName the index name
    * @param terms the terms
    * @param refresh whether to call an index update on ElasticSearch or not
    * @return list of indexing responses
    */
  def indexTermFuture(indexName: String, terms: Terms, refresh: Int) : Future[IndexDocumentListResult] = Future {
    indexTerm(indexName, terms, refresh)
  }

  /** index terms on Elasticsearch
    *
    * @param indexName the index name
    * @param terms the terms
    * @param refresh whether to call an index update on ElasticSearch or not
    * @return list of indexing responses
    */
  def indexTerm(indexName: String, terms: Terms, refresh: Int) : IndexDocumentListResult = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val bulkReq = new BulkRequest()

    terms.terms.foreach( term => {
      val builder : XContentBuilder = jsonBuilder().startObject()

      builder.field("term", term.term)

      term.synonyms match {
        case Some(t) =>
          val indexable: String = payloadMapToString[String, Double](t)
          builder.field("synonyms", indexable)
        case None => ;
      }
      term.antonyms match {
        case Some(t) =>
          val indexable: String = payloadMapToString[String, Double](t)
          builder.field("antonyms", indexable)
        case None => ;
      }
      term.tags match {
        case Some(t) => builder.field("tags", t)
        case None => ;
      }
      term.features match {
        case Some(t) =>
          val indexable: String = payloadMapToString[String, String](t)
          builder.field("features", indexable)
        case None => ;
      }
      term.frequencyBase match {
        case Some(t) => builder.field("frequency_base", t)
        case None => ;
      }
      term.frequencyStem match {
        case Some(t) => builder.field("frequency_stem", t)
        case None => ;
      }
      term.vector match {
        case Some(t) =>
          val indexable_vector: String = payloadVectorToString[Double](t)
          builder.field("vector", indexable_vector)
        case None => ;
      }
      builder.endObject()

      val indexTermReq = new IndexRequest(Index.indexName(indexName, elasticClient.indexSuffix))
        .id(term.term)
        .source(builder)

      bulkReq.add(indexTermReq)
    })

    val bulkResponse: BulkResponse = client.bulk(bulkReq, RequestOptions.DEFAULT)

    val listOfDocRes: List[IndexDocumentResult] = bulkResponse.getItems.map(x => {
      IndexDocumentResult(x.getIndex, x.getId, x.getVersion, x.status === RestStatus.CREATED)
    }).toList

    IndexDocumentListResult(listOfDocRes)
  }

  /** fetch one or more terms from Elasticsearch
    *
    * @param indexName the index name
    * @param termsRequest the ids of the terms to be fetched
    * @return fetched terms
    */
  def termsById(indexName: String,
                termsRequest: DocsIds) : Terms = {
    val documents: List[Term] = if(termsRequest.ids.nonEmpty) {
      val client: RestHighLevelClient = elasticClient.httpClient

      val multiGetReq = new MultiGetRequest()

      termsRequest.ids.foreach{id => multiGetReq.add(
        new MultiGetRequest.Item(Index.indexName(indexName, elasticClient.indexSuffix), id))
      }

      val response: MultiGetResponse = client.mget(multiGetReq, RequestOptions.DEFAULT)

      response.getResponses.toList
        .filter((p: MultiGetItemResponse) => p.getResponse.isExists).map { e =>
        val item: GetResponse = e.getResponse
        val source: Map[String, Any] = item.getSource.asScala.toMap

        val term: String = source.get("term") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val synonyms: Option[Map[String, Double]] = source.get("synonyms") match {
          case Some(t) =>
            val value: String = t.asInstanceOf[String]
            Option {
              payloadStringToMapStringDouble(value)
            }
          case None => None: Option[Map[String, Double]]
        }

        val antonyms: Option[Map[String, Double]] = source.get("antonyms") match {
          case Some(t) =>
            val value: String = t.asInstanceOf[String]
            Option {
              payloadStringToMapStringDouble(value)
            }
          case None => None: Option[Map[String, Double]]
        }

        val tags: Option[String] = source.get("tags") match {
          case Some(t) => Option {
            t.asInstanceOf[String]
          }
          case None => None: Option[String]
        }

        val features: Option[Map[String, String]] = source.get("features") match {
          case Some(t) =>
            val value: String = t.asInstanceOf[String]
            Option {
              payloadStringToMapStringString(value)
            }
          case None => None: Option[Map[String, String]]
        }

        val frequencyBase: Option[Double] = source.get("frequency_base") match {
          case Some(t) => Option {
            t.asInstanceOf[Double]
          }
          case None => None: Option[Double]
        }

        val frequencyStem: Option[Double] = source.get("frequency_stem") match {
          case Some(t) => Option {
            t.asInstanceOf[Double]
          }
          case None => None: Option[Double]
        }

        val vector: Option[Vector[Double]] = source.get("vector") match {
          case Some(t) =>
            val value: String = t.asInstanceOf[String]
            Option {
              payloadStringToDoubleVector(value)
            }
          case None => None: Option[Vector[Double]]
        }

        Term(term = term,
          synonyms = synonyms,
          antonyms = antonyms,
          tags = tags,
          features = features,
          frequencyBase = frequencyBase,
          frequencyStem = frequencyStem,
          vector = vector,
          score = None: Option[Double])
      }
    } else {
      List.empty[Term]
    }

    Terms(terms=documents)
  }

  /** fetch one or more terms from Elasticsearch
    *
    * @param indexName the index name
    * @param termsRequest the ids of the terms to be fetched
    * @return fetched terms
    */
  def getTermsByIdFuture(indexName: String,
                         termsRequest: DocsIds) : Future[Terms] = Future {
    termsById(indexName, termsRequest)
  }

  /** update terms using a Future
    *
    * @param indexName index name
    * @param terms terms to update
    * @param refresh whether to call an index update on ElasticSearch or not
    * @return result of the update operations
    */
  def updateTermFuture(indexName: String, terms: Terms, refresh: Int) : Future[UpdateDocumentsResult] = Future {
    updateTerm(indexName, terms, refresh)
  }

  /** update terms, synchronous function
    *
    * @param indexName index name
    * @param terms terms to update
    * @param refresh whether to call an index update on ElasticSearch or not
    * @return result of the update operations
    */
  private[this] def updateTerm(indexName: String, terms: Terms, refresh: Int) : UpdateDocumentsResult = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val bulkReq : BulkRequest = new BulkRequest()

    terms.terms.foreach( term => {
      val builder : XContentBuilder = jsonBuilder().startObject()

      builder.field("term", term.term)

      term.synonyms match {
        case Some(t) =>
          val indexable: String = payloadMapToString[String, Double](t)
          builder.field("synonyms", indexable)
        case None => ;
      }
      term.antonyms match {
        case Some(t) =>
          val indexable: String = payloadMapToString[String, Double](t)
          builder.field("antonyms", indexable)
        case None => ;
      }
      term.tags match {
        case Some(t) => builder.field("tags", t)
        case None => ;
      }
      term.features match {
        case Some(t) =>
          val indexable: String = payloadMapToString[String, String](t)
          builder.field("features", indexable)
        case None => ;
      }
      term.frequencyBase match {
        case Some(t) => builder.field("frequency_base", t)
        case None => ;
      }
      term.frequencyStem match {
        case Some(t) => builder.field("frequency_stem", t)
        case None => ;
      }
      term.vector match {
        case Some(t) =>
          val indexable_vector: String = payloadVectorToString[Double](t)
          builder.field("vector", indexable_vector)
        case None => ;
      }
      builder.endObject()

      val updateTermReq = new UpdateRequest()
        .index(Index.indexName(indexName, elasticClient.indexSuffix))
        .docAsUpsert(true)
        .id(term.term)
        .doc(builder)

      bulkReq.add(updateTermReq)
    })

    val bulkRes: BulkResponse = client.bulk(bulkReq, RequestOptions.DEFAULT)

    if (refresh =/= 0) {
      val refreshIndex = elasticClient.refresh(Index.indexName(indexName, elasticClient.indexSuffix))
      if(refreshIndex.failedShardsN > 0) {
        throw TermServiceException("Term : index refresh failed: (" + indexName + ")")
      }
    }

    val listOfDocRes: List[UpdateDocumentResult] = bulkRes.getItems.map(x => {
      UpdateDocumentResult(x.getIndex, x.getId, x.getVersion, x.status === RestStatus.CREATED)
    }).toList

    UpdateDocumentsResult(listOfDocRes)
  }

  /** fetch two terms and calculate the distance between them
    *
    * @param indexName the index name
    * @param termsReq list of terms
    * @return the distance between terms
    */
  def termsDistance(indexName: String, termsReq: DocsIds): Future[List[TermsDistanceRes]] = Future {
    val extractedTerms = termsById(indexName, DocsIds(ids = termsReq.ids))
    val retrievedTerms = extractedTerms.terms.map{ t => (t.term, t) }.toMap

    retrievedTerms
      .keys.flatMap(a => retrievedTerms.keys.map(b => (a, b)))
      .filter{case(t1,t2) => t1 =/= t2}.map { case(t1, t2) =>
      val v1 = retrievedTerms(t1).vector.getOrElse(TextToVectorsTools.emptyVec())
      val v2 = retrievedTerms(t2).vector.getOrElse(TextToVectorsTools.emptyVec())
      TermsDistanceRes(
        term1 = t1,
        term2 = t2,
        vector1 = v1,
        vector2 = v2,
        cosDistance = VectorUtils.cosineDist(v1, v2),
        eucDistance = VectorUtils.euclideanDist(v1, v2)
      )
    }.toList
  }

  class StringOrSearchTerm[T]
  object StringOrSearchTerm {
    implicit object SearchTermWitness extends StringOrSearchTerm[SearchTerm]
    implicit object StringWitness extends StringOrSearchTerm[String]
  }

  /** given a text, return all the matching terms
    *
    * @param indexName index term
    * @param query input text or SearchTerm entity
    * @param analyzer the analyzer name to be used for text tokenization
    * @return the terms found
    */
  def search[T: StringOrSearchTerm](indexName: String, query: T,
                                    analyzer: String = "space_punctuation"): TermsResults = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val term_field_name = if (TokenizersDescription.analyzersMap.contains(analyzer))
      "term." + analyzer
    else
      throw TermServiceException("search: analyzer not found or not supported: (" + analyzer + ")")

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .version(true)

    val searchReq = new SearchRequest(Index.indexName(indexName, elasticClient.indexSuffix))
      .source(sourceReq)
      .searchType(SearchType.DFS_QUERY_THEN_FETCH)

    val boolQueryBuilder: BoolQueryBuilder = QueryBuilders.boolQuery()

    query match {
      case term: SearchTerm =>
        term.term match {
          case Some(termProperty) =>
            boolQueryBuilder.must(QueryBuilders.termQuery(term_field_name, termProperty))
          case _ => ;
        }

        term.frequencyBase match {
          case Some(termProperty) =>
            boolQueryBuilder.must(QueryBuilders.termQuery("frequency_base", termProperty))
          case _ => ;
        }

        term.frequencyStem match {
          case Some(termProperty) =>
            boolQueryBuilder.must(QueryBuilders.termQuery("frequency_stem", termProperty))
          case _ => ;
        }

        term.synonyms match {
          case Some(termProperty) =>
            boolQueryBuilder.must(QueryBuilders.termQuery("synonyms", termProperty))
          case _ => ;
        }

        term.antonyms match {
          case Some(termProperty) =>
            boolQueryBuilder.must(QueryBuilders.termQuery("antonyms", termProperty))
          case _ => ;
        }

        term.tags match {
          case Some(termProperty) =>
            boolQueryBuilder.must(QueryBuilders.termQuery("tags", termProperty))
          case _ => ;
        }

        term.features match {
          case Some(termProperty) =>
            boolQueryBuilder.must(QueryBuilders.termQuery("features", termProperty))
          case _ => ;
        }
      case text: String =>
        boolQueryBuilder.should(QueryBuilders.matchQuery(term_field_name, text))
      case _ =>
        throw TermServiceException("Unexpected query type for terms search")
    }

    sourceReq.query(boolQueryBuilder)

    val searchResponse : SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    val documents : List[Term] = searchResponse.getHits.getHits.toList.map { item =>
      val source : Map[String, Any] = item.getSourceAsMap.asScala.toMap

      val term : String = source.get("term") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val synonyms : Option[Map[String, Double]] = source.get("synonyms") match {
        case Some(t) =>
          val value: String = t.asInstanceOf[String]
          Option{payloadStringToMapStringDouble(value)}
        case None => None: Option[Map[String, Double]]
      }

      val antonyms : Option[Map[String, Double]] = source.get("antonyms") match {
        case Some(t) =>
          val value: String = t.asInstanceOf[String]
          Option{payloadStringToMapStringDouble(value)}
        case None => None: Option[Map[String, Double]]
      }

      val tags : Option[String] = source.get("tags") match {
        case Some(t) => Option {t.asInstanceOf[String]}
        case None => None: Option[String]
      }

      val features : Option[Map[String, String]] = source.get("features") match {
        case Some(t) =>
          val value: String = t.asInstanceOf[String]
          Option{payloadStringToMapStringString(value)}
        case None => None: Option[Map[String, String]]
      }

      val frequencyBase : Option[Double] = source.get("frequency_base") match {
        case Some(t) => Option {t.asInstanceOf[Double]}
        case None => None: Option[Double]
      }

      val frequencyStem : Option[Double] = source.get("frequency_stem") match {
        case Some(t) => Option {t.asInstanceOf[Double]}
        case None => None: Option[Double]
      }

      val vector : Option[Vector[Double]] = source.get("vector") match {
        case Some(t) =>
          val value: String = t.asInstanceOf[String]
          Option{payloadStringToDoubleVector(value)}
        case None => None: Option[Vector[Double]]
      }

      Term(term = term,
        synonyms = synonyms,
        antonyms = antonyms,
        tags = tags,
        features = features,
        frequencyBase = frequencyBase,
        frequencyStem = frequencyStem,
        vector = vector,
        score = Option{item.getScore.toDouble})
    }

    val terms: Terms = Terms(terms=documents)

    val maxScore : Float = searchResponse.getHits.getMaxScore
    TermsResults(total = terms.terms.length, maxScore = maxScore, hits = terms)
  }

  /** given a text, return all the matching terms
    *
    * @param indexName index term
    * @param query input text or SearchTerm entity
    * @param analyzer the analyzer name to be used for text tokenization
    * @return the terms found
    */
  def searchFuture[T: StringOrSearchTerm](indexName: String, query: T,
                                          analyzer: String = "space_punctuation"): Future[TermsResults] = Future {
    search(indexName, query, analyzer)
  }

  /** tokenize a text
    *
    * @param indexName index name
    * @param query a TokenizerQueryRequest with the text to tokenize
    * @return a TokenizerResponse with the result of the tokenization
    */
  def esTokenizer(indexName: String, query: TokenizerQueryRequest) : TokenizerResponse = {
    val analyzer = TokenizersDescription.analyzersMap.get(query.tokenizer) match {
      case Some((analyzerEsName, _)) => analyzerEsName
      case _ =>
        throw TermServiceException("esTokenizer: analyzer not found or not supported: (" + query.tokenizer + ")")
    }

    val client: RestHighLevelClient = elasticClient.httpClient

    val analyzerReq = new AnalyzeRequest()
      .index(Index.indexName(indexName, elasticClient.indexSuffix))
      .text(query.text)
      .analyzer(analyzer)

    val analyzeResponse: AnalyzeResponse = client.indices().analyze(analyzerReq, RequestOptions.DEFAULT)

    val tokenizationRes: List[AnalyzeResponse.AnalyzeToken] = if(analyzeResponse.getTokens != null)
      analyzeResponse.getTokens.listIterator.asScala.toList
    else
      List.empty[AnalyzeResponse.AnalyzeToken]

    val tokens : List[TokenizerResponseItem] =
      tokenizationRes.map(t => {
        val responseItem: TokenizerResponseItem =
          TokenizerResponseItem(startOffset = t.getStartOffset,
            position = t.getPosition,
            endOffset = t.getEndOffset,
            token = t.getTerm,
            tokenType = t.getType)
        responseItem
      })

    TokenizerResponse(tokens = tokens)
  }

  /** tokenize a sentence and extract term vectors for each token
    *
    * @param indexName index name
    * @param text input text
    * @param analyzer analyzer name
    * @param unique if true exclude from results duplicated terms
    * @return the TextTerms
    */
  def textToVectors(indexName: String, text: String, analyzer: String = "stop",
                    unique: Boolean = false): TextTerms = {
    val analyzerRequest =
      TokenizerQueryRequest(tokenizer = analyzer, text = text) // analyzer is checked by esTokenizer
    val fullTokenList = esTokenizer(indexName, analyzerRequest).tokens
      .map(e => e.token)

    val tokenList = if (unique) fullTokenList.toSet.toList else fullTokenList
    val termsRequest = DocsIds(ids = tokenList)
    val termList = termsById(indexName, termsRequest)

    val textTerms = TextTerms(text = text,
      textTermsN = tokenList.length,
      termsFoundN = termList.terms.length,
      terms = termList
    )
    textTerms
  }

  /** fetch all documents and serve them through an iterator
    *
    * @param indexName index name
    * @param keepAlive the keep alive timeout for the ElasticSearch document scroller
    * @return an iterator for Items
    */
  def allDocuments(indexName: String, keepAlive: Long = 60000): Iterator[Term] = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .query(QueryBuilders.matchAllQuery)
      .size(100)

    val searchReq = new SearchRequest(Index.indexName(indexName, elasticClient.indexSuffix))
      .source(sourceReq)
      .scroll(new TimeValue(keepAlive))

    var scrollResp: SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)
    val scrollId = scrollResp.getScrollId

    val iterator = Iterator.continually{
      val documents = scrollResp.getHits.getHits.toList.map { e =>
        val source : Map[String, Any] = e.getSourceAsMap.asScala.toMap

        val term : String = source.get("term") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val synonyms : Option[Map[String, Double]] = source.get("synonyms") match {
          case Some(t) =>
            val value: String = t.asInstanceOf[String]
            Option{payloadStringToMapStringDouble(value)}
          case None => None: Option[Map[String, Double]]
        }

        val antonyms : Option[Map[String, Double]] = source.get("antonyms") match {
          case Some(t) =>
            val value: String = t.asInstanceOf[String]
            Option{payloadStringToMapStringDouble(value)}
          case None => None: Option[Map[String, Double]]
        }

        val tags : Option[String] = source.get("tags") match {
          case Some(t) => Option {t.asInstanceOf[String]}
          case None => None: Option[String]
        }

        val features : Option[Map[String, String]] = source.get("features") match {
          case Some(t) =>
            val value: String = t.asInstanceOf[String]
            Option{payloadStringToMapStringString(value)}
          case None => None: Option[Map[String, String]]
        }

        val frequencyBase : Option[Double] = source.get("frequency_base") match {
          case Some(t) => Option {t.asInstanceOf[Double]}
          case None => None: Option[Double]
        }

        val frequencyStem : Option[Double] = source.get("frequency_stem") match {
          case Some(t) => Option {t.asInstanceOf[Double]}
          case None => None: Option[Double]
        }

        val vector : Option[Vector[Double]] = source.get("vector") match {
          case Some(t) =>
            val value: String = t.asInstanceOf[String]
            Option{payloadStringToDoubleVector(value)}
          case None => None: Option[Vector[Double]]
        }

        Term(term = term,
          synonyms = synonyms,
          antonyms = antonyms,
          tags = tags,
          features = features,
          frequencyBase = frequencyBase,
          frequencyStem = frequencyStem,
          vector = vector,
          score = None: Option[Double])
      }

      var scrollRequest: SearchScrollRequest = new SearchScrollRequest(scrollId)
      scrollRequest.scroll(new TimeValue(keepAlive))
      scrollResp = client.scroll(scrollRequest, RequestOptions.DEFAULT)
      (documents, documents.nonEmpty)
    }.takeWhile{case (_, docNonEmpty) => docNonEmpty}
      .flatMap{case (doc, _) => doc}
    iterator
  }

}
