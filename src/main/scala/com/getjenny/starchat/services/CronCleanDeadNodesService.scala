package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 23/01/19.
  */

import akka.actor.{Actor, Props}
import com.getjenny.starchat.SCActorSystem

import scala.concurrent.duration._
import scala.language.postfixOps

object CronCleanDeadNodesService extends CronService {

  class CleanDeadNodesTickActor extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case `tickMessage` =>
        if(clusterNodesService.elasticClient.existsIndices(List(clusterNodesService.indexName))) {
          val cleanedNodes = clusterNodesService.cleanDeadNodes
          if (cleanedNodes.deleted > 0) {
            log.debug("Cleaned " + cleanedNodes.deleted + " nodes: " + cleanedNodes.message)
          }
        } else log.debug("index does not exists: " + clusterNodesService.indexName)
      case _ =>
        log.error("Unknown error cleaning cluster nodes tables")
    }
  }

  def scheduleAction(): Unit = {
    val actorRef =
      SCActorSystem.system.actorOf(Props(new CleanDeadNodesTickActor))
    SCActorSystem.system.scheduler.schedule(
      0 seconds,
      systemIndexManagementService.elasticClient.clusterNodeCleanDeadInterval seconds,
      actorRef,
      tickMessage)
  }
}
