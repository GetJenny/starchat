package com.getjenny.starchat.services

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 23/08/17.
 */

import akka.actor.{Actor, Props}
import com.getjenny.starchat.SCActorSystem

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

/** Download and update the decision tables from elasticsearch
 */
object CronReloadDTService extends CronService {
  protected[this] val indexManagementService: LanguageIndexManagementService.type = LanguageIndexManagementService

  class ReloadAnalyzersTickActor extends Actor {
    protected[this] var updateTimestamp: Long = 0

    def receive: PartialFunction[Any, Unit] = {
      case `tickMessage` =>
        val startUpdateTimestamp: Long = System.currentTimeMillis
        val maxItemsIndexesToUpdate: Long = math.max(analyzerService.dtMaxTables, analyzerService.analyzersMap.size)

        log.debug("Start DT reloading session: {} items({})", startUpdateTimestamp, maxItemsIndexesToUpdate)

        val indexCheck: List[(String, Boolean)] =
          instanceRegistryService.allEnabledInstanceTimestamp(Some(updateTimestamp), Some(maxItemsIndexesToUpdate))
            .map { dtReloadEntry =>
              val indexAnalyzers: Option[ActiveAnalyzers] =
                analyzerService.analyzersMap.get(dtReloadEntry.indexName)
              val localReloadIndexTimestamp = indexAnalyzers match {
                case Some(ts) => ts.lastReloadingTimestamp
                case _ => InstanceRegistryDocument.InstanceRegistryTimestampDefault
              }

              if (dtReloadEntry.timestamp > 0 && localReloadIndexTimestamp < dtReloadEntry.timestamp) {
                log.info("dt reloading for index(" + dtReloadEntry.indexName +
                  ") timestamp (" + startUpdateTimestamp + ") : " + dtReloadEntry.timestamp)
                Try(analyzerService.loadAnalyzers(indexName = dtReloadEntry.indexName)) match {
                  case Success(relRes) =>
                    updateTimestamp = math.max(updateTimestamp, localReloadIndexTimestamp)
                    log.info("Analyzer loaded for index(" + dtReloadEntry + "), timestamp (" +
                      startUpdateTimestamp + ") res(" + relRes + ") remote ts: " + dtReloadEntry)
                    analyzerService.analyzersMap(dtReloadEntry.indexName)
                      .lastReloadingTimestamp = dtReloadEntry.timestamp
                    (dtReloadEntry.indexName, true)
                  case Failure(e) =>
                    log.error("unable to load analyzers for index({}), timestamp({}), cron job: ",
                      dtReloadEntry, startUpdateTimestamp, e)
                    (dtReloadEntry.indexName, false)
                }
              } else {
                (dtReloadEntry.indexName, true)
              }
            }
        indexCheck.filter { case (_, check) => !check }.foreach { case (index, _) =>
          val indexMgmRes = indexManagementService.check(index)
          if (indexMgmRes.check) {
            log.error("Index exists but loading results in an error: " + indexMgmRes.message)
          } else {
            instanceRegistryService.markAsDeleted(ids = List(index))
            log.debug("Deleted update record for the index: " + index)
          }
        }
    }
  }

  def scheduleAction(): Unit = {
    if (nodeDtLoadingStatusService.elasticClient.dtReloadCheckFrequency > 0) {
      val reloadDecisionTableActorRef =
        SCActorSystem.system.actorOf(Props(new ReloadAnalyzersTickActor))
      SCActorSystem.system.scheduler.scheduleWithFixedDelay(
        0 seconds,
        nodeDtLoadingStatusService.elasticClient.dtReloadCheckFrequency seconds,
        reloadDecisionTableActorRef,
        tickMessage)
    }
  }
}
