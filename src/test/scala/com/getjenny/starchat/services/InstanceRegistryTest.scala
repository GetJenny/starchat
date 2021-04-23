package com.getjenny.starchat.services

import com.getjenny.starchat.TestBase
import com.getjenny.starchat.entities.io.CreateLanguageIndexRequest
import com.getjenny.starchat.entities.io.{InstanceRegistryDocument => InstanceRegistryDocumentCC}
import com.getjenny.starchat.services.InstanceRegistryStatus._
import scalaz.Scalaz._

class InstanceRegistryTest extends TestBase {
  val createEnglishRequest = CreateLanguageIndexRequest(List("english"))
  val instanceRegistry: InstanceRegistryService.type = InstanceRegistryService
  val indexName = "index_getjenny_english_0"
  val nonExistentIndex = "index_getjenny_french_0"
  val indexName2 = "index_getjenny_english_common_0"
  val invalidIndexName = "starchat_system.index_test"
  val timestamp = System.currentTimeMillis()

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Post("/language_index_management", createEnglishRequest) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
  }

  override protected def afterAll(): Unit = {
    Delete(s"/language_index_management?index_name=index_english") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
    InstanceRegistryService.markAsDeleted(List("index_getjenny_english_0", "index_getjenny_english_common_0"))
    super.afterAll()
  }

  "Instance Registry" should {

    "fail if index name is not valid" in {
      intercept[Exception] {
        instanceRegistry.addInstance(invalidIndexName)
      }
    }

    "fail if index not exists" in {
      intercept[Exception] {
        instanceRegistry.addInstance(nonExistentIndex)
      }
    }

    "map fields returned by elastic" in {
      val sourceMap = Map(
        "timestamp" -> 0L,
        "enabled" -> true,
        "delete" -> false,
        "deleted" -> false
      )
      val document = InstanceRegistryDocument(sourceMap)
      document.enabled.isDefined shouldEqual true
      document.delete.isDefined shouldEqual true
      document.deleted.isDefined shouldEqual true
    }

    "evaluate correctly statuses" in {
      InstanceRegistryDocumentCC().status() shouldEqual Missing
      InstanceRegistryDocumentCC(deleted = true.some).status() shouldEqual Missing
      InstanceRegistryDocumentCC(enabled = false.some, deleted = true.some).status shouldEqual Missing

      intercept[Exception] {
        InstanceRegistryDocumentCC(deleted = false.some).status()
      }

      InstanceRegistryDocumentCC(enabled = true.some).status() shouldEqual Enabled
      InstanceRegistryDocumentCC(enabled = true.some, delete = false.some).status() shouldEqual Enabled
      InstanceRegistryDocumentCC(enabled = false.some).status() shouldEqual Disabled
      InstanceRegistryDocumentCC(enabled = false.some, delete = false.some).status() shouldEqual Disabled
      InstanceRegistryDocumentCC(enabled = false.some, delete = true.some).status() shouldEqual MarkedForDeletion
      InstanceRegistryDocumentCC(enabled = true.some, delete = true.some).status() shouldEqual MarkedForDeletion
      InstanceRegistryDocumentCC(enabled = true.some, delete = true.some, deleted = false.some)
        .status() shouldEqual MarkedForDeletion
      InstanceRegistryDocumentCC(delete = true.some).status() shouldEqual MarkedForDeletion
    }

    "evaluate correctly is empty" in {
      InstanceRegistryDocumentCC().isEmpty shouldBe true
      InstanceRegistryDocumentCC(deleted = true.some).isEmpty shouldBe true
      InstanceRegistryDocumentCC(deleted = true.some, enabled = true.some).isEmpty shouldBe true
      InstanceRegistryDocumentCC(deleted = true.some, delete = true.some).isEmpty shouldBe true
      InstanceRegistryDocumentCC(deleted = true.some, timestamp = 0L.some).isEmpty shouldBe true
    }

    "add an instance" in {
      instanceRegistry.addInstance(indexName)
      instanceRegistry.addInstance(indexName2)

      val instance = instanceRegistry.getInstance(indexName)
      val instance2 = instanceRegistry.getInstance(indexName2)

      instance.enabled.isDefined shouldBe true
      instance.enabled shouldEqual Some(true)
      instance.status() shouldEqual Enabled
      instance2.enabled.isDefined shouldBe true
      instance2.enabled shouldEqual Some(true)
      instance2.status() shouldEqual Enabled
    }

    "fail if trying to search an invalid index name" in {
      intercept[Exception] {
        instanceRegistry.getInstance(invalidIndexName)
      }
    }

    "fail if trying to update instance with invalid index name" in {
      intercept[Exception] {
        instanceRegistry.disableInstance(invalidIndexName)
      }
    }

    "enable instance" in {
      instanceRegistry.enableInstance(indexName)

      val instance = instanceRegistry.getInstance(indexName)
      instance.enabled.isDefined shouldEqual true
      instance.enabled shouldEqual Some(true)
      instance.status() shouldEqual Enabled
    }

    "disable instance" in {
      instanceRegistry.disableInstance(indexName)

      val instance = instanceRegistry.getInstance(indexName)
      instance.enabled.isDefined shouldEqual true
      instance.enabled shouldEqual Some(false)
      instance.status() shouldEqual Disabled
    }

    "try to create instance that already exists" in {
      intercept[Exception] {
        instanceRegistry.addInstance(indexName)
      }
    }

    "mark delete instance" in {
      instanceRegistry.markDeleteInstance(indexName)

      val instance = instanceRegistry.getInstance(indexName)
      instance.delete.isDefined shouldEqual true
      instance.delete shouldEqual Some(true)
      instance.status() shouldEqual MarkedForDeletion
    }

    "try to create instance that already exists and marked to delete" in {
      intercept[Exception] {
        instanceRegistry.addInstance(indexName)
      }
    }

    "delete entry" in {
      instanceRegistry.markAsDeleted(List(indexName))

      val instance = instanceRegistry.getInstance(indexName)

      instance.isEmpty shouldEqual true
      instance.status() shouldEqual Missing
    }

    "recreate entry" in {
      instanceRegistry.addInstance(indexName)

      val instance = instanceRegistry.getInstance(indexName)
      instance.isEmpty shouldEqual false
      instance.status() shouldEqual Enabled
    }

    "update timestamp" in {
      instanceRegistry.updateTimestamp(indexName, timestamp)

      val instance = instanceRegistry.instanceTimestamp(indexName)

      instance.timestamp shouldEqual timestamp
    }

    "get all instances" in {

      val allInstances = instanceRegistry.getAll()

      allInstances.nonEmpty shouldEqual true
      allInstances.size shouldEqual 2
    }

    "get all enabled instances timestamp" in {

      instanceRegistry.enableInstance(indexName)

      val allInstances = instanceRegistry.allEnabledInstanceTimestamp()

      allInstances.nonEmpty shouldEqual true


      allInstances.size shouldEqual 2

      val instancesMap = allInstances.map(x => x.indexName -> x.timestamp).toMap

      instancesMap.get(indexName2) shouldEqual Some(0)
    }
  }

}
