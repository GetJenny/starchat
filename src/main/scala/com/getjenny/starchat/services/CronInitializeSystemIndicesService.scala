package com.getjenny.starchat.services

import com.getjenny.starchat.SCActorSystem
import akka.actor.{Actor, Props}
import scala.concurrent.duration._
import scala.language.postfixOps

object CronInitializeSystemIndicesService extends CronService {

  class InitializeSystemIndicesActor extends Actor {
    val client = systemIndexManagementService.elasticClient
    val indices = List(
      client.indexName + "." + client.userIndexSuffix,
      client.indexName + "." + client.systemRefreshDtIndexSuffix,
      client.indexName + "." + client.systemClusterNodesIndexSuffix,
      client.indexName + "." + client.systemDtNodesStatusIndexSuffix
    )

    def receive: PartialFunction[Any, Unit] = {
      case `tickMessage` =>
        if(client.existsIndices(indices))
          log.info("System indices exist")
        else {
          log.info("System indices are missing, initializing system indices")
          systemIndexManagementService.create()
        }
      case _ =>
        log.error("Unknown error initializing system indices.")
    }
  }

  def scheduleAction(): Unit = {
    val actorRef =
      SCActorSystem.system.actorOf(Props(new InitializeSystemIndicesActor))
    SCActorSystem.system.scheduler.scheduleOnce(
      0 seconds,
      actorRef,
      tickMessage)
  }
}
