package com.getjenny.starchat.analyzer.atoms.http

import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.stream.Materializer
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import com.getjenny.starchat.analyzer.atoms.http.AuthorizationType.AuthorizationType
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants.ParameterName.parameterConstantList
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants.Regex.templateRegex
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants._
import com.getjenny.starchat.analyzer.atoms.http.StoreOption.StoreOption
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._
import scalaz.syntax.std.option.ToOptionOpsFromOption
import scalaz.{Failure, NonEmptyList, Reader, Success, Validation, ValidationNel}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.Try

case class VariableManagerException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
  * To be implemented if one does not use the generic variable manager.
  *
  * It defines methods to create a `HttpRequestAtomicConfiguration`.
  *
  */
trait  VariableManager {
  type AtomValidation[T] = ValidationNel[String, T]

  private[this] val keyValueSeparator = "="
  private[this] val queryKeyword = "query"

  /**
    * Used to create Url conf. Same for authenticationConf. Typically not to be reimplemented
    * @param configuration map of all configuration values
    * @param findProperty see findIn
    * @return
    */
  def urlConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomUrlConf]

  /**
    * As per urlConf
    *
    */
  def authenticationConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[Option[HttpAtomAuthConf]]

  /**
    * Build e.g. the json or the query string for the http service.
    *
    * @param configuration
    * @param findProperty
    * @return
    */
  def inputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[Option[HttpAtomInputConf]]


  def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf]

  def confParamsList: List[String] = configurationPrefix.map{
    prefix => parameterConstantList.map(p => s"$prefix.$p")
  }.getOrElse(Nil)

  /** Prefix of the implemented atomic (eg http-atom.weather).
    *
    * To be implemented in each custom atomic
    *
    * @return
    */
  def configurationPrefix: Option[String]

  /**
    * Called by HttpRequestAtomic. Checks that all arguments needed by to create
    * an `HttpRequestAtomicConfiguration` are there. These arguments' value are passed
    * through restrictedArgs (system configuration) extractedVariables (passed as atomic
    * argument etc), query.
    *
    * Not to be re-implemented
    *
    * @param arguments arguments in the atomic
    * @param restrictedArgs
    * @param extractedVariables
    * @param query
    * @return
    */
  def validateAndBuild(arguments: List[String],
                       restrictedArgs: Map[String, String],
                       extractedVariables: Map[String, String],
                       query: String): AtomValidation[HttpRequestAtomicConfiguration] = {
    val allVariables = arguments ++ confParamsList
    val argumentConfiguration = createArgumentConfiguration(allVariables)
    val runtimeConfiguration = (argumentConfiguration ++ extractedVariables) + (queryKeyword -> query)
    val findProperty: String => Option[String] = findIn(systemConfiguration = restrictedArgs, runtimeConfiguration)
    val configuration = variableConfiguration(allVariables.map(_.split(keyValueSeparator)(0)), findProperty)

    (urlConf(configuration, findProperty) |@|
      authenticationConf(configuration, findProperty) |@|
      inputConf(configuration, findProperty) |@|
      outputConf(configuration, findProperty)
      ) {
      (url, auth, qs, out) => HttpRequestAtomicConfiguration(url, auth, qs, out)
    }
  }

  /**
  * This is useful when a specific http atom implementation allows passing parameter as arguments
  * e.g weather atom builded with specific location paramter weather("location=London")
  * override this returning an empty map if this behaviour is not needed
  */
  protected[this] def createArgumentConfiguration(arguments: List[String]): Map[String, String] = {
    arguments
      .filter(_.contains(keyValueSeparator))
      .map { x =>
        val keyValue = x.split(keyValueSeparator)
        keyValue(0) -> keyValue(1)
      }.toMap
  }

  def variableConfiguration(arguments: List[String],
                            findProperty: String => Option[String]): VariableConfiguration = {
    arguments
      .map(variable => variable.substring(variable.lastIndexOf('.') + 1, variable.length) -> findProperty(variable))
      .collect { case (key, Some(value)) => (key, value) }
      .toMap
  }

  /**
    *
    * @param systemConfiguration The config file
    * @param runtimeConfiguration Eg argument of the atomic, extracted variables, query...
    * @return a function that can be used to get configuration values from system or if not from runtime Configuration
    */
  protected final def findIn(systemConfiguration: Map[String, String], runtimeConfiguration: Map[String, String])
                            (key: String): Option[String] = {
    systemConfiguration.get(key)
      .orElse(runtimeConfiguration.get(key))
  }

  private[this] def evaluateTemplate(template: String,
                                     findProperty: String => Option[String]): String = {
    templateRegex.findAllIn(template)
      .foldLeft(template) { case (acc, variable) =>
        findProperty(variable)
          .map { v =>
            val regex = s"\\$templateDelimiterPrefix$variable\\$templateDelimiterSuffix"
            acc.replaceAll(regex, v)
          }
          .getOrElse(acc)
      }
  }

  /**
    *
    *
    * @param template
    * @param findProperty
    * @return
    */
  protected def substituteTemplate(template: String,
                                   findProperty: String => Option[String]): AtomValidation[String] = {
    val formatted = evaluateTemplate(template, findProperty)
    if (formatted.contains(templateDelimiterPrefix)) {
      Failure(NonEmptyList(s"Unable to found substitution in template: $template"))
    } else {
      Success(formatted)
    }
  }


}

object AtomVariableReader {
  type VariableConfiguration = Map[String, String]

  type VariableReader[T] = Reader[VariableConfiguration, ValidationNel[String, T]]

  def keyFromMap(configMap: VariableConfiguration, key: String): Validation[String, String] = {
    configMap.get(key)
      .map(Success(_))
      .getOrElse(Failure(s"$key not found in configuration"))
  }

  def as[T](key: String)(implicit to: String => VariableReader[T]): VariableReader[T] = {
    to(key)
  }

  implicit def asString(key: String): VariableReader[String] = {
    Reader((request: VariableConfiguration) =>
      keyFromMap(request, key)
        .toValidationNel)
  }

  implicit def asAuthorizationType(key: String): VariableReader[AuthorizationType] = {
    Reader((request: VariableConfiguration) =>
      keyFromMap(request, key)
        .flatMap({
          AuthorizationType.fromName(_) |> toMessage[AuthorizationType](key)
        })
        .toValidationNel)
  }

  implicit def asHttpMethod(key: String): VariableReader[HttpMethod] = {
    Reader((request: VariableConfiguration) =>
      keyFromMap(request, key)
        .flatMap(x => {
          HttpMethods.getForKey(x)
            .toSuccess(s"Error while extracting key <$key>: $x is an invalid method")
        })
        .toValidationNel)
  }

  private[this] def getContentType(contentType: String): Validation[String, ContentType] = {
    ContentType.parse(contentType).leftMap(_.map(_.summary).mkString(";")).validation
  }

  implicit def asContentType(key: String): VariableReader[ContentType] = {
    Reader((request: VariableConfiguration) =>
      keyFromMap(request, key)
        .flatMap(getContentType)
        .toValidationNel)
  }

  implicit def asStoreOption(key: String): VariableReader[StoreOption] = {
    Reader((request: VariableConfiguration) =>
      keyFromMap(request, key)
        .flatMap({
          StoreOption.fromName(_) |> toMessage[StoreOption](key)
        })
        .toValidationNel)
  }

  def toMessage[S](key: String)(v: Validation[Throwable, S]): Validation[String, S] = {
    ((f: Throwable) => s"Error while extracting key <$key>: ${f.getMessage}") <-: v
  }
}

/**
  * Contains all parameters needed to execute an http call
  *
  * @param urlConf
  * @param auth
  * @param inputConf
  * @param outputConf
  */
case class HttpRequestAtomicConfiguration(urlConf: HttpAtomUrlConf,
                                          auth: Option[HttpAtomAuthConf],
                                          inputConf: Option[HttpAtomInputConf],
                                          outputConf: HttpAtomOutputConf)

case class HttpAtomUrlConf(url: String, method: HttpMethod, contentType: ContentType)

object AuthorizationType extends Enumeration {
  type AuthorizationType = Value
  val BASIC: AuthorizationType = Value("basic")
  val BEARER: AuthorizationType = Value("bearer")
  val API_KEY: AuthorizationType = Value("apiKey")

  def fromName(s: String): Validation[Throwable, AuthorizationType] = Try(withName(s)).toValidation
}

object StoreOption extends Enumeration {
  type StoreOption = Value
  val HEADER: StoreOption = Value("header")
  val QUERY: StoreOption = Value("query")

  def fromName(s: String): Validation[Throwable, StoreOption] = Try(withName(s)).toValidation
}

trait HttpAtomAuthConf

case class BasicAuth(username: String, password: String) extends HttpAtomAuthConf

case class BearerAuth(token: String) extends HttpAtomAuthConf

case class ApiKeyAuth(key: String, token: String, storeTo: StoreOption) extends HttpAtomAuthConf

trait HttpAtomInputConf

case class QueryStringConf(queryString: String) extends HttpAtomInputConf

case class JsonConf(json: String) extends HttpAtomInputConf

trait HttpAtomOutputConf {
  val score: String

  def responseExtraction(response: HttpResponse)
                        (implicit ec: ExecutionContext, materializer: Materializer): Future[Map[String, String]] = {
    Unmarshaller.stringUnmarshaller(response.entity)
      .map(bodyParser(_, response.entity.contentType.toString(), response.status))
  }

  def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String]

  def exists(extractedVariables: Map[String, String]): Boolean = {
    extractedVariables.contains(score)
  }

  def getScoreValue(extractedVariables: Map[String, String]): Int = {
    extractedVariables.getOrElse(score, "0").toInt
  }
}




