package com.getjenny.command

/**
  * Created by angelo on 05/09/18.
  */

import java.io._

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.getjenny.starchat.serializers.JsonSupport
import edu.stanford.nlp.process.DocumentPreprocessor
import scalaz.Scalaz._
import scopt.OptionParser

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.ExecutionContextExecutor
import scala.io.Source

object TextToDtFeatures extends JsonSupport {

  private[this] case class Params(
                                   inputfile: String = "file.txt",
                                   out_dictionary: String = "dictionary.tsv",
                                   out_training: String = "training.data",
                                   terms_filter: String = "",
                                   out_names: String = "features.names",
                                   filter: String = """([\p{L}|\d|_| |/]+)""",
                                   add_feature_id: Boolean = true,
                                   omit_null_feature: Boolean = false,
                                   window: Int = 5,
                                   center: Int = 2,
                                   take: Int = 0 // take up to N sentences
                                 )

  private[this] def execute(params: Params) {
    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val termsFilter = if(params.terms_filter.nonEmpty) {
      val bufferedSource = Source.fromFile(params.terms_filter)
      val termsSet = bufferedSource.getLines.map(_.toLowerCase).toSet
      bufferedSource.close
      termsSet
    } else {
      Set[String]()
    }

    val reader: FileReader = new FileReader(params.inputfile)
    val docProcecessor: DocumentPreprocessor = new DocumentPreprocessor(reader)
    val sentencesIt = docProcecessor.iterator.asScala
    val tokenizedSentencesUnfiltered0 = sentencesIt.map (sentenceItem =>
      sentenceItem.asScala.map(token => token.word.toLowerCase).toVector
    ).map(sentenceItem => sentenceItem.filter( ! _.matches("""([^\p{L}|^\d])""")))

    val tokenizedSentencesUnfiltered1 = if(params.filter.nonEmpty)
      tokenizedSentencesUnfiltered0.filter(v => ! v.exists(! _.matches(params.filter)))
    else
      tokenizedSentencesUnfiltered0

    val tokenizedSentencesUnfiltered2 = if(termsFilter.nonEmpty)
      tokenizedSentencesUnfiltered1.filter(v => ! v.exists(t => ! termsFilter.contains(t)))
    else
      tokenizedSentencesUnfiltered1

    val tokenizedSentencesUnfiltered3 = if(params.take > 0)
      tokenizedSentencesUnfiltered2.take(params.take)
    else
      tokenizedSentencesUnfiltered2

    val tokenizedSentences = tokenizedSentencesUnfiltered3

    val nullFeature: Long = 0
    val dict = mutable.Map[String, Long]()
    if(! params.omit_null_feature) dict("") = nullFeature // empty feature
    val sentenceFeatures = tokenizedSentences.map (sentenceItem =>
      sentenceItem.map (tokenItem =>
        dict.get(tokenItem) match {
          case Some(i) => i
          case _ =>
            val newIndex = dict.size.toLong
            dict(tokenItem) = newIndex
            newIndex
        }
      )
    ).filter(_.length > 1)

    val window = params.window
    val center = params.center

    val leftPad = center
    val rightPad = window - center + 1
    val features = sentenceFeatures.map { sentenceItem =>
      val paddedSentence = Vector.fill(leftPad)(nullFeature) ++ sentenceItem ++ Vector.fill(rightPad)(nullFeature)
      paddedSentence.sliding(window).filter(v => v(center) =/= nullFeature)
    }.flatMap { vectorsItem =>
      vectorsItem.map { vectorItem =>
        val centerValue = vectorItem(center)
        vectorItem.patch(center, List[Long](), 1) ++ Vector[Long](centerValue)
      }
    }

    val featuresWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(params.out_training)))
    if(params.add_feature_id) {
      features.zipWithIndex.map{ case (featuresItem, index) => featuresItem ++ Vector(index.toLong)}
    } else {
      features
    }.foreach(row => {
      val entry = row.mkString(",")
      featuresWriter.write(entry)
      featuresWriter.newLine()
    })
    featuresWriter.close()

    val dictionaryWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(params.out_dictionary)))
    dict.foreach { case(key, value) =>
      dictionaryWriter.write(key + "\t" + value)
      dictionaryWriter.newLine()
    }
    dictionaryWriter.close()

    val dictSize = dict.size

    val names: List[String] = List("target.    | the target attribute") ++
      List.range(0, window -1).map(name =>
        "e" + name + ":\t\t\t\t" + List.range(0, dictSize).mkString(",") + "."
      ) ++ List("target:\t\t\t\t" + List.range(0, dictSize).mkString(",") + ".") ++
      List("ID:\t\t\t\tlabel.")

    val namesWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(params.out_names)))
    names.foreach { line =>
      namesWriter.write(line)
      namesWriter.newLine()
    }
    namesWriter.close()
  }

  def main(args: Array[String]) {
    val defaultParams = Params()
    val parser = new OptionParser[Params]("IndexSubtitles") {
      head("Index data into stat_text")
      help("help").text("prints this usage text")
      opt[String]("inputfile")
        .text(s"the path of the file with the subtitles" +
          s"  default: ${defaultParams.inputfile}")
        .action((x, c) => c.copy(inputfile = x))
      opt[String]("out_dictionary")
        .text(s"the output file where to save the dictionary" +
          s"  default: ${defaultParams.out_dictionary}")
        .action((x, c) => c.copy(out_dictionary = x))
      opt[String]("out_training")
        .text(s"the output file where to save the training data" +
          s"  default: ${defaultParams.out_training}")
        .action((x, c) => c.copy(out_training = x))
      opt[String]("out_names")
        .text(s"the output file where to save the features names" +
          s"  default: ${defaultParams.out_names}")
        .action((x, c) => c.copy(out_names = x))
      opt[String]("filter")
        .text(s"a regex to filter sentences which matches, an empty regex means no filtering" +
          s"  default: ${defaultParams.filter}")
        .action((x, c) => c.copy(filter = x))
      opt[String]("terms_filter")
        .text(s"a file with valid terms, all the other will be removed to reduce the dataset" +
          s"  default is empty")
        .action((x, c) => c.copy(terms_filter = x))
      opt[Boolean]("add_feature_id")
        .text(s"add a progressive feature id at the end of the feature line" +
          s"  default is  ${defaultParams.add_feature_id}")
        .action((x, c) => c.copy(add_feature_id = x))
      opt[Boolean]("omit_null_feature")
        .text(s"omit the null feature on dictionary)" +
          s"  default is  ${defaultParams.omit_null_feature}")
        .action((x, c) => c.copy(omit_null_feature = x))
      opt[Int]("take")
        .text(s"take up to N filtered sentences" +
          s"  default: ${defaultParams.take}")
        .action((x, c) => c.copy(take = x))
      opt[Int]("window")
        .text(s"the size of the sliding window " +
          s"  default: ${defaultParams.window}")
        .action((x, c) => c.copy(window = x))
      opt[Int]("center")
        .text(s"the index of the center of the window starting from 0" +
          s"  default: ${defaultParams.center}")
        .action((x, c) => c.copy(center = x))
    }

    parser.parse(args, defaultParams) match {
      case Some(params) =>
        execute(params)
        sys.exit(0)
      case _ =>
        sys.exit(1)
    }
  }
}
