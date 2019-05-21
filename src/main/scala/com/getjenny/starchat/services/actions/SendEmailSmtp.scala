package com.getjenny.starchat.services.actions

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 26/04/19.
  */

import com.getjenny.starchat.entities.DtActionResult
import courier.Defaults._
import courier._
import javax.mail.internet.InternetAddress

import scala.concurrent.Await
import scala.concurrent.duration._


object SendEmailSmtp extends DtAction {
  override def apply(indexName: String, stateName: String, params: Map[String, String]): DtActionResult = {

    val host: String = params.get("host") match {
      case Some(v) => v
      case _ => throw DtActionException("Field missing on SendEmailSmtp Action: host")
    }

    val port: Int = params.getOrElse("port", "587").toInt

    val username: String = params.get("username") match {
      case Some(v) => v
      case _ => throw DtActionException("Field missing on SendEmailSmtp Action: username")
    }

    val password: String = params.get("password") match {
      case Some(v) => v
      case _ => throw DtActionException("Field missing on SendEmailSmtp Action: password")
    }

    val mailer = Mailer(host, port)
      .auth(true)
      .as(user = username, pass = password)
      .startTls(true)()

    val from: InternetAddress = params.get("from") match {
      case Some(v) => new InternetAddress(v)
      case _ => throw DtActionException("Field missing on SendEmailSmtp Action: from")
    }

    var envelope: Envelope = Envelope.from(from)

    params.get("to") match {
      case Some(v) =>
        v.replace(" ", "").split(";")
        .map(addr => new InternetAddress(addr))
        .foreach(addr => {
          envelope = envelope.to(addr)
        })
      case _ =>
    }

    params.get("replyTo") match {
      case Some(v) => envelope = envelope.replyTo(new InternetAddress(v))
      case _ =>
    }

    params.get("cc") match {
      case Some(v) => v.replace(" ", "").split(";")
        .map(addr => new InternetAddress(addr))
        .foreach(addr => envelope = envelope.cc(addr))
      case _ =>
    }

    params.get("bcc") match {
      case Some(v) => v.replace(" ", "").split(";")
        .map(addr => new InternetAddress(addr))
        .foreach(addr => envelope = envelope.bcc(addr))
      case _ =>
    }

    val html: String = params.getOrElse("html", "false")
    val body: String = params.getOrElse("body", "")

    params.get("subject") match {
      case Some(v) => envelope = envelope.subject(v)
      case _ => throw DtActionException("Field missing on SendEmailSmtp Action: subject")
    }

    val content: Content = html match {
      case "true" => Multipart().html(body)
      case _ => Text(body)
    }

    envelope = envelope.content(content)

    val sendRes = mailer(envelope)

    val actionRes = sendRes map { _ =>
      DtActionResult(success = true)
    } recover {
      case e: Exception =>
        log.error("Error sending email through SendEmailSmtp index(" +
          indexName + ") stateName(" + stateName + ") Envelope{" + envelope.toString + "}", e.getMessage)
        DtActionResult(code = 1)
    }
    Await.result(actionRes, 5.second)
  }
}