package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 23/01/19.
  */

import akka.actor.{Actor, Props}
import com.getjenny.starchat.SCActorSystem

import scala.concurrent.duration._
import scala.language.postfixOps

object CronNodeAliveSignalService extends CronService {

  class NodeAliveSignalTickActor extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case `tickMessage` =>
        if(clusterNodesService.elasticClient.existsIndices(List(clusterNodesService.indexName))) {
          clusterNodesService.alive()
        } else log.debug("index does not exists: " + clusterNodesService.indexName)
      case _ =>
        log.error("Unknown error communicating that node is alive")
    }
  }

  def scheduleAction(): Unit = {
    val actorRef =
      SCActorSystem.system.actorOf(Props(new NodeAliveSignalTickActor))
    SCActorSystem.system.scheduler.schedule(
      0 seconds,
      systemIndexManagementService.elasticClient.clusterNodeAliveSignalInterval seconds,
      actorRef,
      tickMessage)
  }

}
