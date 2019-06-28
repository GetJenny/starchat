package com.getjenny.command

/**
  * Created by angelo on 28/09/17.
  */

import java.io.{File, FileInputStream}

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpRequest, _}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import com.getjenny.starchat.entities._
import com.getjenny.starchat.serializers.JsonSupport
import scopt.OptionParser

import scala.collection.immutable
import scala.collection.immutable.List
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}

object IndexDecisionTableJSON extends JsonSupport {

  private[this] case class Params(
                             host: String = "http://localhost:8888",
                             indexName: String = "index_getjenny_english_0",
                             path: String = "/decisiontable",
                             inputfile: String = "decision_table.json",
                             separator: Char = ',',
                             skiplines: Int = 1,
                             timeout: Int = 60,
                             numcols: Int = 11,
                             headerKv: Seq[String] = Seq.empty[String]
                           )

  private[this] def execute(params: Params) {
    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val baseUrl = params.host + "/" + params.indexName + params.path
    val file = new File(params.inputfile)
    val stream = new FileInputStream(file)
    val json = scala.io.Source.fromInputStream(stream).mkString

//    val listOfDocumentsRes: Try[SearchDTDocumentsResults] =
    val listOfDocuments = Await.ready(Unmarshal(json).to[SearchDTDocumentsResults], 30.seconds).value match {
        case Some(listOfDocumentsRes) => listOfDocumentsRes match {
          case Success(dtDocumentResult) => dtDocumentResult
          case Failure(e) =>
            println("Error: " + e)
            SearchDTDocumentsResults(total = 0, maxScore = .0f, hits = List.empty[SearchDTDocument])
        }
        case _ =>
          println("Error: empty response")
          SearchDTDocumentsResults(total = 0, maxScore = .0f, hits = List.empty[SearchDTDocument])
    }

    val httpHeader: immutable.Seq[HttpHeader] = if(params.headerKv.nonEmpty) {
      val headers: Seq[RawHeader] = params.headerKv.map(x => {
        val headerOpt = x.split(":")
        val key = headerOpt(0)
        val value = headerOpt(1)
        RawHeader(key, value)
      }) ++ Seq(RawHeader("application", "json"))
      headers.to[immutable.Seq]
    } else {
      immutable.Seq(RawHeader("application", "json"))
    }

    val timeout = Duration(params.timeout, "s")

    listOfDocuments.hits.foreach(item => {
      val entry = item.document
      val state = DTDocument(state = entry.state,
        executionOrder = entry.executionOrder,
        maxStateCount = entry.maxStateCount,
        analyzer = entry.analyzer,
        queries = entry.queries,
        bubble = entry.bubble,
        action = entry.action,
        actionInput = entry.actionInput,
        stateData = entry.stateData,
        successValue = entry.successValue,
        failureValue = entry.failureValue,
        evaluationClass = entry.evaluationClass
      )

      val entity_future = Marshal(state).to[MessageEntity]
      val entity = Await.result(entity_future, 10.second)
      val responseFuture: Future[HttpResponse] =
        Http().singleRequest(HttpRequest(
          method = HttpMethods.POST,
          uri = baseUrl,
          headers = httpHeader,
          entity = entity))
      val result = Await.result(responseFuture, timeout)
      result.status match {
        case StatusCodes.Created | StatusCodes.OK => println("indexed: " + state.state)
        case _ =>
          println("failed indexing state(" + state.state + ") Message(" + result.toString() + ")")
      }
    })
    Await.ready(system.terminate(), Duration.Inf)
  }

  def main(args: Array[String]) {
    val defaultParams = Params()
    val parser = new OptionParser[Params]("IndexDecisionTable") {
      head("Index data into DecisionTable from a JSON file")
      help("help").text("prints this usage text")
      opt[String]("inputfile")
        .text(s"the path of the file with the vectors" +
          s"  default: ${defaultParams.inputfile}")
        .action((x, c) => c.copy(inputfile = x))
      opt[String]("host")
        .text(s"*Chat base url" +
          s"  default: ${defaultParams.host}")
        .action((x, c) => c.copy(host = x))
      opt[String]("index_name")
        .text(s"the index_name, e.g. index_XXX" +
          s"  default: ${defaultParams.indexName}")
        .action((x, c) => c.copy(indexName = x))
      opt[String]("path")
        .text(s"the service path" +
          s"  default: ${defaultParams.path}")
        .action((x, c) => c.copy(path = x))
      opt[Int]("timeout")
        .text(s"the timeout in seconds of each insert operation" +
          s"  default: ${defaultParams.timeout}")
        .action((x, c) => c.copy(timeout = x))
      opt[Int]("skiplines")
        .text(s"skip the first N lines from vector file" +
          s"  default: ${defaultParams.skiplines}")
        .action((x, c) => c.copy(skiplines = x))
      opt[Seq[String]]("header_kv")
        .text(s"header key-value pair, as key1:value1,key2:value2" +
          s"  default: ${defaultParams.headerKv}")
        .action((x, c) => c.copy(headerKv = x))
    }

    parser.parse(args, defaultParams) match {
      case Some(params) =>
        execute(params)
      case _ =>
        sys.exit(1)
    }
  }
}
