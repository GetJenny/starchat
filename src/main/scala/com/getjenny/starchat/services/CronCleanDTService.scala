package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 23/08/17.
  */

import akka.actor.{Actor, Props}
import com.getjenny.starchat.SCActorSystem

import scala.concurrent.duration._
import scala.language.postfixOps

object CronCleanDTService extends CronService {

  class CleanDecisionTablesTickActor extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case `tickMessage` =>
        if(analyzerService.dtMaxTables > 0 &&
          analyzerService.analyzersMap.size > analyzerService.dtMaxTables ) {
          val exceedingItems: Long = analyzerService.dtMaxTables - analyzerService.analyzersMap.size
          val itemsToRemove =
            analyzerService.analyzersMap.toList.sortBy{
              case (_, analyzer) => analyzer.lastEvaluationTimestamp
            }.take(exceedingItems.toInt)
          itemsToRemove.foreach{case(state, _)=>
            log.info("removing decision table: " + state)
            analyzerService.analyzersMap.remove(state)
          }
        }
      case _ =>
        log.error("Unknown error cleaning decision tables")
    }
  }

  def scheduleAction(): Unit = {
    val actorRef =
      SCActorSystem.system.actorOf(Props(new CleanDecisionTablesTickActor))

    SCActorSystem.system.scheduler.schedule(
      0 seconds,
      30 seconds,
      actorRef,
      tickMessage)
  }

}
