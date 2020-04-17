package com.getjenny.starchat.services.auth

import akka.http.scaladsl.server.directives.Credentials
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.io.Permissions.Permission
import com.getjenny.starchat.entities.io.User

import scala.concurrent.{ExecutionContext, Future}

case class AuthenticatorException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

abstract class AbstractStarChatAuthenticator {
  implicit def executionContext: ExecutionContext = SCActorSystem.system.dispatcher

  def fetchUser(id: String): Future[User]

  def authenticator(credentials: Credentials): Future[Option[User]]

  def hasPermissions(user: User, index: String, permissions: Set[Permission]): Future[Boolean]

  def hasPermissions(user: User, index: String, permission: Permission): Future[Boolean] = {
    hasPermissions(user = user, index = index, permissions = Set(permission))
  }

  def secret(password: String, salt: String): String

  def hashedSecret(password: String, salt: String): String
}
