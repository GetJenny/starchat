package com.getjenny.starchat.services

import java.io.{File, FileInputStream, FileReader}

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import breeze.io.CSVReader
import com.getjenny.starchat.entities.{DTDocument, SearchDTDocument, SearchDTDocumentsResults, Term}
import com.getjenny.starchat.serializers.JsonSupport
import scalaz.Scalaz._

import scala.collection.immutable.{List, Map}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}

case class FileToDocumentsException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

object FileToDocuments extends JsonSupport {

  def getDTDocumentsFromJSON(log: LoggingAdapter, file: File): IndexedSeq[DTDocument] = {
    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val stream = if(file.isAbsolute) {
      new FileInputStream(file)
    } else {
      throw FileToDocumentsException("file path must be absolute: " + file.getPath)
    }

    val json = scala.io.Source.fromInputStream(stream).mkString

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

    listOfDocuments.hits.map { doc =>
        doc.document
    }.toIndexedSeq
  }

  def getDTDocumentsFromCSV(log: LoggingAdapter, file: File, skipLines: Int = 0, separator: Char = ','):
  IndexedSeq[DTDocument] = {
    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val fileReader = if(file.isAbsolute) {
      new FileReader(file)
    } else {
      throw FileToDocumentsException("file path must be absolute: " + file.getPath)
    }

    lazy val fileEntries = CSVReader.read(input=fileReader, separator=separator,
      quote = '"', skipLines=skipLines)

    val header = fileEntries.headOption match {
      case Some(t) => t.zipWithIndex.toMap
      case _ => throw FileToDocumentsException("empty or malformed file: " + file.getPath)
    }

    fileEntries.tail.map { entry =>
      if (entry.length =/= header.length) {
        val message = "file row is not consistent entry(" + entry.length + ") != header(" + header.length +
          ") Row(" + entry.toString + ")"
        throw new Exception(message)
      } else {
        val queriesCsvString = entry(header("queries"))
        val actionInputCsvString = entry(header("actionInput"))
        val stateDataCsvString = entry(header("stateData"))

        val queriesFuture: Future[List[String]] = queriesCsvString match {
          case "" => Future { List.empty[String] }
          case _ => Unmarshal(queriesCsvString).to[List[String]]
        }

        val actionInputFuture: Future[Map[String, String]] = actionInputCsvString match {
          case "" => Future { Map.empty[String, String] }
          case _ => Unmarshal(actionInputCsvString).to[Map[String, String]]
        }

        val stateDataFuture: Future[Map[String, String]] = stateDataCsvString match {
          case "" => Future { Map.empty[String, String] }
          case _ => Unmarshal(stateDataCsvString).to[Map[String, String]]
        }

        val queries = Await.result(queriesFuture, 10.second)
        val actionInput = Await.result(actionInputFuture, 10.second)
        val stateData = Await.result(stateDataFuture, 10.second)
        val evaluationClass: Option[String] = if(entry.contains("evaluationClass")) {
          Some{entry(header("evaluationClass"))}
        } else None

        val document = DTDocument(state = entry(header("state")),
          executionOrder = entry(header("executionOrder")).toInt,
          maxStateCount = entry(header("maxStateCount")).toInt,
          analyzer = entry(header("analyzer")),
          queries = queries,
          bubble = entry(header("bubble")),
          action = entry(header("action")),
          actionInput = actionInput,
          stateData = stateData,
          successValue = entry(header("successValue")),
          failureValue = entry(header("failureValue")),
          evaluationClass =  evaluationClass
        )

        document
      }
    }
  }

  def getTermsDocumentsFromCSV(log: LoggingAdapter, file: File, skipLines: Int = 0, separator: Char = ','):
  IndexedSeq[Term] = {
    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val fileReader = if(file.isAbsolute) {
      new FileReader(file)
    } else {
      throw FileToDocumentsException("file path must be absolute: " + file.getPath)
    }

    lazy val fileEntries = CSVReader.read(input=fileReader, separator=separator,
      quote = '"', skipLines=skipLines)

    val header = fileEntries.headOption match {
      case Some(t) => t.zipWithIndex.toMap
      case _ => throw FileToDocumentsException("empty or malformed file: " + file.getPath)
    }
    fileEntries.tail.map(entry => {
      if (entry.length =/= header.length) {
        val message = "file row is not consistent (" + entry.length + "!=" + header.length +
          ") Row(" + entry.toString + ")"
        throw FileToDocumentsException(message)
      } else {
        //type,term,associatedTerms,score
        val termType = entry(header("type"))
        val term = entry(header("term"))
        val associatedTerms = entry(header("associatedTerms"))
          .split(";").map( entry => entry.split(":"))
          .map(x => (x(0), x(1).toDouble)).toMap

        val document = if(termType === "SYN") {
          Term(
            term = term,
            synonyms = Some(associatedTerms)
          )
        } else if(termType === "ANT") {
          Term(
            term = term,
            antonyms = Some(associatedTerms)
          )
        } else {
          throw FileToDocumentsException(message = "associated term type not recognized: " + termType)
        }

        document
      }
    })
  }

}
