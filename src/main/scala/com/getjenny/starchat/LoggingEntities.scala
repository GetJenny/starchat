package com.getjenny.starchat

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 27/06/16.
  */

import java.util.Base64

import akka.event.Logging
import akka.http.scaladsl.model.{HttpRequest, _}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.{DebuggingDirectives, LogEntry}
import akka.http.scaladsl.server.{Directive0, RouteResult}
import com.typesafe.config.{Config, ConfigFactory}

object LoggingEntities {
  val config: Config = ConfigFactory.load()

  val remoteAddressReqUriReqRes = "remoteAddress(%1$s) ReqUri(%2$s) ReqMethodRes(%3$s:%4$s)"

  def address(remoteAddress: RemoteAddress): String = remoteAddress.toIP match {
    case Some(addr) => addr.ip.getHostAddress
    case _ => "unknown ip"
  }

  def requestMethodAndResponseStatusReduced(remoteAddress: RemoteAddress, requestTimestamp: Long)
                                           (req: HttpRequest): RouteResult => Option[LogEntry] = {
    case RouteResult.Complete(res) =>
      val responseTimestamp: Long = System.currentTimeMillis()
      val elapsedTime: Long = (responseTimestamp - requestTimestamp)
      Some(LogEntry(
        remoteAddressReqUriReqRes.format(address(remoteAddress), req.uri, req.method.name, res.status) +
          " ElapsedTimeMs(" + elapsedTime + ")",
        Logging.InfoLevel))
    case RouteResult.Rejected(rejections) =>
      val responseTimestamp: Long = System.currentTimeMillis()
      val elapsedTime: Long = (responseTimestamp - requestTimestamp)
      Some(LogEntry("remoteAddress(" + address(remoteAddress) +
        ") Rejected: " + rejections.mkString(", ") +
        " ElapsedTimeMs(" + elapsedTime + ")", Logging.DebugLevel))
    case _ =>
      val responseTimestamp: Long = System.currentTimeMillis()
      val elapsedTime: Long = (responseTimestamp - requestTimestamp)
      Some(LogEntry("remoteAddress(" + address(remoteAddress) +
        ") : Unknown RouteResult" +
        " ElapsedTimeMs(" + elapsedTime + ")", Logging.ErrorLevel))
  }

  def requestMethodAndResponseStatus(remoteAddress: RemoteAddress, requestTimestamp: Long)
                                    (req: HttpRequest): RouteResult => Option[LogEntry] = {
    case RouteResult.Complete(res) =>
      val responseTimestamp: Long = System.currentTimeMillis()
      val elapsedTime: Long = (responseTimestamp - requestTimestamp)
      Some(LogEntry(
        remoteAddressReqUriReqRes.format(address(remoteAddress), req.uri, req.method.name, res.status) +
        " ReqEntity(" + req.entity.httpEntity + ") ResEntity(" + res.entity + ") ElapsedTimeMs(" + elapsedTime + ")"
        , Logging.InfoLevel))
    case RouteResult.Rejected(rejections) =>
      val responseTimestamp: Long = System.currentTimeMillis()
      val elapsedTime: Long = (responseTimestamp - requestTimestamp)
      Some(LogEntry("remoteAddress(" + address(remoteAddress) +
        ") Rejected: " + rejections.mkString(", ") + " ElapsedTimeMs(" + elapsedTime + ")", Logging.DebugLevel))
    case _ =>
      val responseTimestamp: Long = System.currentTimeMillis()
      val elapsedTime: Long = (responseTimestamp - requestTimestamp)
      Some(LogEntry("remoteAddress(" + address(remoteAddress) +
        ") : Unknown RouteResult" + " ElapsedTimeMs(" + elapsedTime + ")", Logging.ErrorLevel))
  }

  def requestMethodAndResponseStatusB64(remoteAddress: RemoteAddress, requestTimestamp: Long)
                                       (req: HttpRequest): RouteResult => Option[LogEntry] = {
    case RouteResult.Complete(res) =>
      val responseTimestamp: Long = System.currentTimeMillis()
      val elapsedTime: Long = (responseTimestamp - requestTimestamp)
      Some(LogEntry(
        remoteAddressReqUriReqRes.format(address(remoteAddress), req.uri, req.method.name, res.status) +
        " ReqEntity(" + req.entity + ")" +
        " ReqB64Entity(" + Base64.getEncoder.encodeToString(req.entity.toString.getBytes) + ")" +
        " ResEntity(" + res.entity + ")" +
        " ResB64Entity(" + Base64.getEncoder.encodeToString(res.entity.toString.getBytes) + ")" +
        " ElapsedTimeMs(" + elapsedTime + ")", Logging.InfoLevel))
    case RouteResult.Rejected(rejections) =>
      val responseTimestamp: Long = System.currentTimeMillis()
      val elapsedTime: Long = (responseTimestamp - requestTimestamp)
      Some(LogEntry("remoteAddress(" + address(remoteAddress) +
        ") Rejected: " + rejections.mkString(", ") +
        " ElapsedTimeMs(" + elapsedTime + ")", Logging.DebugLevel))
    case _ =>
      val responseTimestamp: Long = System.currentTimeMillis()
      val elapsedTime: Long = (responseTimestamp - requestTimestamp)
      Some(LogEntry("remoteAddress(" + address(remoteAddress) +
        ") : Unknown RouteResult" +
        " ElapsedTimeMs(" + elapsedTime + ")", Logging.ErrorLevel))
  }

  val logRequestAndResult: Directive0 =
    extractClientIP.flatMap { ip =>
      val requestTimestamp: Long = System.currentTimeMillis()
      DebuggingDirectives.logRequestResult(requestMethodAndResponseStatus(ip, requestTimestamp) _)
    }

  val logRequestAndResultB64: Directive0 =
    extractClientIP.flatMap { ip =>
      val requestTimestamp: Long = System.currentTimeMillis()
      DebuggingDirectives.logRequestResult(requestMethodAndResponseStatusB64(ip, requestTimestamp) _)
    }

  val logRequestAndResultReduced: Directive0 =
    extractClientIP.flatMap { ip =>
      val requestTimestamp: Long = System.currentTimeMillis()
      DebuggingDirectives.logRequestResult(requestMethodAndResponseStatusReduced(ip, requestTimestamp) _)
    }
}



