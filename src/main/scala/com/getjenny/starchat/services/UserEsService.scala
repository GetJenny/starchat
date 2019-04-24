package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 01/12/17.
  */

import com.getjenny.analyzer.util.RandomNumbers
import com.getjenny.starchat.entities._
import com.getjenny.starchat.services.auth.AbstractStarChatAuthenticator
import com.getjenny.starchat.services.esclient.SystemIndexManagementElasticClient
import com.typesafe.config.{Config, ConfigFactory}
import javax.naming.AuthenticationException
import org.elasticsearch.action.delete.{DeleteRequest, DeleteResponse}
import org.elasticsearch.action.get.{GetResponse, _}
import org.elasticsearch.action.index.{IndexRequest, IndexResponse}
import org.elasticsearch.action.update.{UpdateRequest, UpdateResponse}
import org.elasticsearch.client.{RequestOptions, RestHighLevelClient}
import org.elasticsearch.common.xcontent.XContentBuilder
import org.elasticsearch.common.xcontent.XContentFactory._
import org.elasticsearch.rest.RestStatus
import scalaz.Scalaz._

import scala.collection.JavaConverters._
import scala.concurrent.Future

case class UserEsServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
  * Implements functions, eventually used by IndexManagementResource, for ES index management
  */
class UserEsService extends AbstractUserService {
  private[this] val config: Config = ConfigFactory.load()
  private[this] val elasticClient: SystemIndexManagementElasticClient.type = SystemIndexManagementElasticClient
  private[this] val indexName: String = elasticClient.indexName + "." + elasticClient.userIndexSuffix

  private[this] val admin: String = config.getString("starchat.basic_http_es.admin")
  private[this] val password: String = config.getString("starchat.basic_http_es.password")
  private[this] val salt: String = config.getString("starchat.basic_http_es.salt")
  private[this] val adminUser = User(id = admin, password = password, salt = salt,
    permissions = Map("admin" -> Set(Permissions.admin)))

  def create(user: User): Future[IndexDocumentResult] = Future {

    if(user.id === "admin") {
      throw new AuthenticationException("admin user cannot be changed")
    }

    val builder : XContentBuilder = jsonBuilder().startObject()

    builder.field("id", user.id)
    builder.field("password", user.password)
    builder.field("salt", user.salt)

    val permissions = builder.startObject("permissions")
    user.permissions.foreach{case(permIndexName, userPermissions) =>
      val array = permissions.field(permIndexName).startArray()
      userPermissions.foreach(p => { array.value(p.toString)}) // for each permission
      array.endArray()
    }
    permissions.endObject()

    builder.endObject()

    val client: RestHighLevelClient = elasticClient.httpClient

    val indexReq = new IndexRequest()
      .index(indexName)
      .create(true)
      .id(user.id)
      .source(builder)

    val response: IndexResponse = client.index(indexReq, RequestOptions.DEFAULT)

    val refreshIndex = elasticClient.refresh(indexName)
    if(refreshIndex.failedShardsN > 0) {
      throw UserEsServiceException("User : index refresh failed: (" + indexName + ")")
    }

    val docResult: IndexDocumentResult = IndexDocumentResult(index = response.getIndex,
      id = response.getId,
      version = response.getVersion,
      created = response.status === RestStatus.CREATED
    )

    docResult
  }

  def update(user: UserUpdate): Future[UpdateDocumentResult] = Future {

    if(user.id === "admin") {
      throw new AuthenticationException("admin user cannot be changed")
    }

    val builder : XContentBuilder = jsonBuilder().startObject()

    user.password match {
      case Some(t) => builder.field("password", t)
      case None => ;
    }

    user.salt match {
      case Some(t) => builder.field("salt", t)
      case None => ;
    }

    user.permissions match {
      case Some(_) =>
        val permissions = builder.startObject("permissions")
        user.permissions.getOrElse(Map.empty).foreach {
          case(permIndexName, userPermissions) =>
            val array = permissions.field(permIndexName).startArray()
            userPermissions.foreach(p => { array.value(p.toString)})
            array.endArray()
        }
        permissions.endObject()
      case None => ;
    }

    builder.endObject()

    val client: RestHighLevelClient = elasticClient.httpClient

    val updateReq = new UpdateRequest()
      .index(indexName)
      .doc(builder)
      .id(user.id)

    val response: UpdateResponse = client.update(updateReq, RequestOptions.DEFAULT)

    val refresh_index = elasticClient.refresh(indexName)
    if(refresh_index.failedShardsN > 0) {
      throw UserEsServiceException("User : index refresh failed: (" + indexName + ")")
    }

    val docResult: UpdateDocumentResult = UpdateDocumentResult(index = response.getIndex,
      id = response.getId,
      version = response.getVersion,
      created = response.status === RestStatus.CREATED
    )

    docResult
  }

  def delete(user: UserId): Future[DeleteDocumentResult] = Future {

    if(user.id === "admin") {
      throw new AuthenticationException("admin user cannot be changed")
    }

    val client: RestHighLevelClient = elasticClient.httpClient

    val deleteReq = new DeleteRequest()
      .index(indexName)
      .id(user.id)

    val response: DeleteResponse = client.delete(deleteReq, RequestOptions.DEFAULT)

    val refreshIndex = elasticClient.refresh(indexName)
    if(refreshIndex.failedShardsN > 0) {
      throw new Exception("User: index refresh failed: (" + indexName + ")")
    }

    val docResult: DeleteDocumentResult = DeleteDocumentResult(index = response.getIndex,
      id = response.getId,
      version = response.getVersion,
      found = response.status =/= RestStatus.NOT_FOUND
    )

    docResult
  }

  def read(user: UserId): Future[User] = Future {
    if(user.id === "admin") {
      adminUser
    } else {

      val client: RestHighLevelClient = elasticClient.httpClient

      val getReq = new GetRequest()
        .index(indexName)
        .id(user.id)

      val response: GetResponse = client.get(getReq, RequestOptions.DEFAULT)
      val source = if(response.getSource != None.orNull) {
        response.getSource.asScala.toMap
      } else {
        throw UserEsServiceException("Cannot find user: " + id)
      }

      val userId: String = source.get("id") match {
        case Some(t) => t.asInstanceOf[String]
        case None => throw UserEsServiceException("User id field is empty for: " + id)
      }

      val password: String = source.get("password") match {
        case Some(t) => t.asInstanceOf[String]
        case None => throw UserEsServiceException("Password is empty for the user: " + id)
      }

      val salt: String = source.get("salt") match {
        case Some(t) => t.asInstanceOf[String]
        case None => throw UserEsServiceException("Salt is empty for the user: " + id)
      }

      val permissions: Map[String, Set[Permissions.Value]] = source.get("permissions") match {
        case Some(t) => t.asInstanceOf[java.util.HashMap[String, java.util.List[String]]]
          .asScala.map{case(permIndexName, userPermissions) =>
          (permIndexName, userPermissions.asScala.map(permissionString => Permissions.value(permissionString)).toSet)
        }.toMap
        case None =>
          throw UserEsServiceException("Permissions list is empty for the user: " + id)
      }
      User(id = userId, password = password, salt = salt, permissions = permissions)
    }
  }

  /** given id and optionally password and permissions, generate a new user */
  def genUser(user: UserUpdate, authenticator: AbstractStarChatAuthenticator): Future[User] = Future {

    val passwordPlain = user.password match {
      case Some(t) => t
      case None =>
        generatePassword()
    }

    val salt = user.salt match {
      case Some(t) => t
      case None =>
        generateSalt()
    }

    val password = authenticator.hashedSecret(password = passwordPlain, salt = salt)

    val permissions = user.permissions match {
      case Some(t) => t
      case None =>
        Map.empty[String, Set[Permissions.Value]]
    }

    User(id = user.id, password = password, salt = salt, permissions = permissions)
  }

  def generatePassword(size: Int = 16): String = {
    RandomNumbers.string(size)
  }

  def generateSalt(): String = {
    RandomNumbers.string(16)
  }
}

