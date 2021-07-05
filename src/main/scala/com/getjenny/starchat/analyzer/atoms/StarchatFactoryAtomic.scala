package com.getjenny.starchat.analyzer.atoms

/**
 * Created by mal on 20/02/2017.
 */

import com.getjenny.analyzer.atoms._
import com.getjenny.analyzer.interfaces._
import com.getjenny.starchat.analyzer.atoms.http.custom._
import com.getjenny.starchat.analyzer.atoms.http.{GenericVariableManager, HttpRequestAtomic}

class StarchatFactoryAtomic extends AtomicFactoryTrait[List[String], AbstractAtomic, Map[String, String]] {

  override val operations: Set[String] = Set(
    "checkDate",
    "checkDayOfMonth",
    "checkDayOfWeek",
    "checkHour",
    "checkMinute",
    "checkMonth",
    "checkMultipleDaysOfWeek",
    "checkTimestamp",
    "checkTimestampVariable",
    "checkVariableValue",
    "checkVariableContainsValue",
    "cosDistanceKeywords",
    "distance",
    "doubleNumberVariable",
    "efecteApi",
    "existsVariable",
    "getLimeData",
    "hasTravState",
    "hasTravStateInPosition",
    "hasTravStateInPositionRev",
    "httpRequest",
    "isServiceOpen",
    "keyword",
    "languageGuesser",
    "lastTravStateIs",
    "ltCustomerInfo",
    "ltCustomerWorksiteNo",
    "matchDateDDMMYYYY",
    "matchPatternRegex",
    "parseDate",
    "parseName",
    "plugin",
    "prevTravStateIs",
    "readRemoteData",
    "readSheetsProductsByFeature",
    "readSheetsProductsByName",
    "regex",
    "search",
    "setServiceOpening",
    "similar",
    "similarCosEmd",
    "similarCosEmdState",
    "similarEucEmd",
    "similarEucEmdState",
    "similarState",
    "submitHubspot",
    "synonym",
    "synonymCosine",
    "timeBetween",
    "toDouble",
    "vOneKeyword",
    "weather",
    "entityExtractor",
    "zendeskTicketComments",
    "zendeskSearchTickets"
  )

  override def get(name: String, argument: List[String], restrictedArgs: Map[String, String]):
  AbstractAtomic = name.filter(c => !c.isWhitespace) match {
    case "checkDate" => new CheckDateAtomic(argument, restrictedArgs)
    case "checkDayOfMonth" => new CheckDayOfMonthAtomic(argument, restrictedArgs)
    case "checkDayOfWeek" => new CheckDayOfWeekAtomic(argument, restrictedArgs)
    case "checkHour" => new CheckHourAtomic(argument, restrictedArgs)
    case "checkMinute" => new CheckMinuteAtomic(argument, restrictedArgs)
    case "checkMonth" => new CheckMonthAtomic(argument, restrictedArgs)
    case "checkMultipleDaysOfWeek" => new CheckMultipleDaysOfWeekAtomic(argument, restrictedArgs)
    case "checkTimestamp" => new CheckTimestampAtomic(argument, restrictedArgs)
    case "checkTimestampVariable" => new CheckTimestampVariableAtomic(argument, restrictedArgs)
    case "checkVariableValue" => new CheckVariableValue(argument, restrictedArgs)
    case "checkVariableContainsValue" => new CheckVariableContainsValue(argument, restrictedArgs)
    case "distance" | "cosDistanceKeywords" => new CosineDistanceAnalyzer(argument, restrictedArgs)
    case "doubleNumberVariable" => new DoubleNumberVariableAtomic(argument, restrictedArgs)
    case "efecteApi" => new HttpRequestAtomic(argument, restrictedArgs) with UHEfecteApiVariableManager
    case "entityExtractor" => new HttpRequestAtomic(argument, restrictedArgs) with EntityExtractorVariableManager
    case "existsVariable" => new ExistsVariableAtomic(argument, restrictedArgs)
    case "getLimeData" => new HttpRequestAtomic(argument, restrictedArgs) with GetLimeDataVariableManager
    case "hasTravState" => new HasTravStateAtomic(argument, restrictedArgs)
    case "hasTravStateInPosition" => new HasTravStateInPositionAtomic(argument, restrictedArgs)
    case "hasTravStateInPositionRev" => new HasTravStateInPositionRevAtomic(argument, restrictedArgs)
    case "httpRequest" => new HttpRequestAtomic(argument, restrictedArgs) with GenericVariableManager
    case "isServiceOpen" => new IsServiceOpenAtomic(argument, restrictedArgs)
    case "keyword" => new KeywordAtomic2(argument, restrictedArgs)
    case "languageGuesser" => new LanguageGuesserAtomic(argument, restrictedArgs)
    case "lastTravStateIs" => new LastTravStateIsAtomic(argument, restrictedArgs)
    case "ltCustomerInfo" => new HttpRequestAtomic(argument, restrictedArgs) with LTCustomerInfoVariableManager
    case "ltCustomerWorksiteNo" => new HttpRequestAtomic(argument, restrictedArgs) with LTCustomerWorksiteInfoVariableManager
    case "matchDateDDMMYYYY" => new MatchDateDDMMYYYYAtomic(argument, restrictedArgs)
    case "matchPatternRegex" => new MatchPatternRegexAtomic(argument, restrictedArgs)
    case "parseDate" => new HttpRequestAtomic(argument, restrictedArgs) with ParseDateVariableManager
    case "parseName" => new HttpRequestAtomic(argument, restrictedArgs) with ParseNameVariableManager
    case "plugin" => new HttpRequestAtomic(argument, restrictedArgs) with PluginVariableManager
    case "prevTravStateIs" => new PrevTravStateIsAtomic(argument, restrictedArgs)
    case "readRemoteData" => new HttpRequestAtomic(argument, restrictedArgs) with ReadS3DataVariableManager
    case "readSheetsProductsByName" => new HttpRequestAtomic(argument, restrictedArgs) with ReadSheetsProductsByNameVariableManager
    case "readSheetsProductsByFeature" => new HttpRequestAtomic(argument, restrictedArgs) with ReadSheetsProductsByFeatureVariableManager
    case "zendeskTicketComments" => new HttpRequestAtomic(argument, restrictedArgs) with ZendeskTicketCommentsVariableManager
    case "zendeskSearchTickets" => new HttpRequestAtomic(argument, restrictedArgs) with ZendeskSearchTicketsVariableManager
    case "regex" => new RegularExpressionAtomic(argument, restrictedArgs)
    case "search" => new SearchAtomic(argument, restrictedArgs)
    case "setServiceOpening" => new SetServiceOpeningAtomic(argument, restrictedArgs)
    case "similar" => new W2VCosineSentenceAtomic(argument, restrictedArgs)
    case "similarCosEmd" => new W2VEarthMoversCosineDistanceAtomic(argument, restrictedArgs)
    case "similarCosEmdState" => new W2VEarthMoversCosineDistanceStateAtomic(argument, restrictedArgs)
    case "similarEucEmd" => new W2VEarthMoversEuclideanDistanceAtomic(argument, restrictedArgs)
    case "similarEucEmdState" => new W2VEarthMoversEuclideanDistanceStateAtomic(argument, restrictedArgs)
    case "similarState" => new W2VCosineStateAtomic(argument, restrictedArgs)
    case "submitHubspot" => new HttpRequestAtomic(argument, restrictedArgs) with SubmitHubspotVariableManager
    case "synonym" => new W2VCosineWordAtomic(argument, restrictedArgs)
    case "timeBetween" => new TimeBetweenAtomic(argument, restrictedArgs)
    case "toDouble" => new ToDoubleNumberAtomic(argument, restrictedArgs)
    case "vOneKeyword" => new KeywordAtomic(argument, restrictedArgs)
    case "weather" => new HttpRequestAtomic(argument, restrictedArgs) with WeatherVariableManager
    case "zendeskTicketComments" => new HttpRequestAtomic(argument, restrictedArgs) with ZendeskTicketCommentsVariableManager
    case "zendeskSearchTickets" => new HttpRequestAtomic(argument, restrictedArgs) with ZendeskSearchTicketsVariableManager
    case _ => throw ExceptionAtomic("Atom \'" + name + "\' not found")
  }
}
