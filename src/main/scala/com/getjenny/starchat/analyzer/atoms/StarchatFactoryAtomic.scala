package com.getjenny.starchat.analyzer.atoms

/**
  * Created by mal on 20/02/2017.
  */

import com.getjenny.analyzer.atoms._
import com.getjenny.analyzer.interfaces._

class StarchatFactoryAtomic extends AtomicFactoryTrait[List[String], AbstractAtomic, Map[String, String]] {

  override val operations: Set[String] = Set(
    "keyword",
    "vOneKeyword",
    "regex",
    "search",
    "synonym",
    "synonymCosine",
    "similar",
    "similarState",
    "similarEucEmd",
    "similarEucEmdState",
    "similarCosEmd",
    "similarCosEmdState",
    "matchPatternRegex",
    "matchDateDDMMYYYY",
    "existsVariable",
    "hasTravState",
    "lastTravStateIs",
    "prevTravStateIs",
    "hasTravStateInPosition",
    "hasTravStateInPositionRev",
    "cosDistanceKeywords",
    "distance",
    "checkTimestamp",
    "checkDayOfWeek",
    "checkDayOfMonth",
    "checkMonth",
    "checkHour",
    "checkMinute",
    "doubleNumberVariable",
    "toDouble",
    "checkTimestampVariable",
    "isServiceOpen",
    "setServiceOpening",
    "languageGuesser"
  )

  override def get(name: String, argument: List[String], restrictedArgs: Map[String, String]):
                  AbstractAtomic = name.filter(c => !c.isWhitespace ) match {
    case "keyword" => new KeywordAtomic2(argument, restrictedArgs)
    case "vOneKeyword" => new KeywordAtomic(argument, restrictedArgs)
    case "regex" => new RegularExpressionAtomic(argument, restrictedArgs)
    case "search" => new SearchAtomic(argument, restrictedArgs)
    case "synonym" => new W2VCosineWordAtomic(argument, restrictedArgs)
    case "similar" => new W2VCosineSentenceAtomic(argument, restrictedArgs)
    case "similarState" => new W2VCosineStateAtomic(argument, restrictedArgs)
    case "similarEucEmd" => new W2VEarthMoversEuclideanDistanceAtomic(argument, restrictedArgs)
    case "similarEucEmdState" => new W2VEarthMoversEuclideanDistanceStateAtomic(argument, restrictedArgs)
    case "similarCosEmd" => new W2VEarthMoversCosineDistanceAtomic(argument, restrictedArgs)
    case "similarCosEmdState" => new W2VEarthMoversCosineDistanceStateAtomic(argument, restrictedArgs)
    case "matchPatternRegex" => new MatchPatternRegexAtomic(argument, restrictedArgs)
    case "matchDateDDMMYYYY" => new MatchDateDDMMYYYYAtomic(argument, restrictedArgs)
    case "existsVariable" => new ExistsVariableAtomic(argument, restrictedArgs)
    case "hasTravState" => new HasTravStateAtomic(argument, restrictedArgs)
    case "lastTravStateIs" => new LastTravStateIsAtomic(argument, restrictedArgs)
    case "prevTravStateIs" => new PrevTravStateIsAtomic(argument, restrictedArgs)
    case "hasTravStateInPosition" => new HasTravStateInPositionAtomic(argument, restrictedArgs)
    case "hasTravStateInPositionRev" => new HasTravStateInPositionRevAtomic(argument, restrictedArgs)
    case "distance" | "cosDistanceKeywords" => new CosineDistanceAnalyzer(argument, restrictedArgs)
    case "checkTimestamp" => new CheckTimestampAtomic(argument, restrictedArgs)
    case "checkDayOfWeek" => new CheckDayOfWeekAtomic(argument, restrictedArgs)
    case "checkDayOfMonth" => new CheckDayOfMonthAtomic(argument, restrictedArgs)
    case "checkMonth" => new CheckMonthAtomic(argument, restrictedArgs)
    case "checkHour" => new CheckHourAtomic(argument, restrictedArgs)
    case "checkMinute" => new CheckMinuteAtomic(argument, restrictedArgs)
    case "doubleNumberVariable" => new DoubleNumberVariableAtomic(argument, restrictedArgs)
    case "toDouble" => new ToDoubleNumberAtomic(argument, restrictedArgs)
    case "checkTimestampVariable" => new CheckTimestampVariableAtomic(argument, restrictedArgs)
    case "isServiceOpen" => new IsServiceOpenAtomic(argument, restrictedArgs)
    case "setServiceOpening" => new SetServiceOpeningAtomic(argument, restrictedArgs)
    case "languageGuesser" => new LanguageGuesserAtomic(argument, restrictedArgs)
    case _ => throw ExceptionAtomic("Atom \'" + name + "\' not found")
  }
}
