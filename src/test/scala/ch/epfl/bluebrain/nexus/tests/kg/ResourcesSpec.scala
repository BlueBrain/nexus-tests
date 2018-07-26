package ch.epfl.bluebrain.nexus.tests.kg

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.{RequestEntity, StatusCodes, HttpRequest => Req}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, Inspectors}

class ResourcesSpec extends BaseSpec with Eventually with Inspectors with CancelAfterFailure {

  val orgId   = genId()
  val projId1 = genId()
  val id1     = s"$orgId/$projId1"

  "cleaning ACLs" should {
    "delete all ACLs for user" in cleanAcls
  }

  "creating projects" should {

    "add projects/create, orgs/create, orgs/write, resources/create, resources/read, resources/write  permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> """projects/create","projects/read","orgs/write","resources/create","resources/read","resources/write","orgs/create""")
      ).toEntity
      cl(Req(PUT, s"$iamBase/acls/", headersGroup, json)).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
        result.entity.isKnownEmpty() shouldEqual true
      }
    }

    "succeed if payload is correct" in {
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersUser, orgReqEntity())).mapResp { result =>
        result.status shouldEqual StatusCodes.Created
      }

      cl(Req(PUT, s"$adminBase/projects/$id1", headersUser, kgReqEntity())).mapResp { result =>
        result.status shouldEqual StatusCodes.Created
      }
    }
  }

  "adding schema" should {
    "create a schema" in {
      val schemaPayload = jsonContentOf("/kg/schemas/simple-schema.json")

      eventually {
        cl(Req(PUT, s"$kgBase/schemas/$id1/test-schema", headersUser, schemaPayload.toEntity)).mapResp { result =>
          result.status shouldEqual StatusCodes.Created
        }
      }
    }
  }

  "creating a resource" should {
    "succeed if the payload is correct" in {
      val payload = jsonContentOf("/kg/resources/simple-resource.json", Map(quote("{priority}") -> "5"))

      eventually {
        cl(Req(PUT, s"$kgBase/resources/$id1/test-schema/test-resource:1", headersUser, payload.toEntity)).mapResp {
          result =>
            result.status shouldEqual StatusCodes.Created
        }
      }
    }

    "fetch the payload" in {
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1", headersUser)).mapJson { (json, result) =>
        val expected = jsonContentOf("/kg/resources/simple-resource-response.json",
                                     Map(quote("{priority}") -> "5", quote("{rev}") -> "1"))
        result.status shouldEqual StatusCodes.OK
        json.removeField("_createdAt").removeField("_updatedAt") shouldEqual expected
      }
    }
  }

  "updating a resource" should {
    "send the update" in {
      val payload = jsonContentOf("/kg/resources/simple-resource.json", Map(quote("{priority}") -> "3"))

      cl(Req(PUT, s"$kgBase/resources/$id1/test-schema/test-resource:1?rev=1", headersUser, payload.toEntity)).mapResp {
        result =>
          result.status shouldEqual StatusCodes.OK
      }
    }
    "fetch the update" in {
      val expected = jsonContentOf("/kg/resources/simple-resource-response.json",
                                   Map(quote("{priority}") -> "3", quote("{rev}") -> "2"))
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json.removeField("_createdAt").removeField("_updatedAt") shouldEqual expected
      }
    }

    "fetch previous revision" in {
      val expected = jsonContentOf("/kg/resources/simple-resource-response.json",
                                   Map(quote("{priority}") -> "5", quote("{rev}") -> "1"))
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1?rev=1", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json.removeField("_createdAt").removeField("_updatedAt") shouldEqual expected
      }
    }
  }

  "tagging a resource" should {

    "create a tag" in {
      val tag1 = jsonContentOf("/kg/resources/tag.json", Map(quote("{tag}") -> "v1.0.0", quote("{rev}") -> "1"))
      val tag2 = jsonContentOf("/kg/resources/tag.json", Map(quote("{tag}") -> "v1.0.1", quote("{rev}") -> "2"))

      cl(Req(PUT, s"$kgBase/resources/$id1/test-schema/test-resource:1/tags?rev=2", headersUser, tag1.toEntity))
        .mapResp { resp =>
          resp.status shouldEqual StatusCodes.Created
        }
      cl(Req(PUT, s"$kgBase/resources/$id1/test-schema/test-resource:1/tags?rev=3", headersUser, tag2.toEntity))
        .mapResp { resp =>
          resp.status shouldEqual StatusCodes.Created
        }

    }
    "fetch a tagged value" in {

      val exptectedTag1 = jsonContentOf("/kg/resources/simple-resource-response.json",
                                        Map(quote("{priority}") -> "3", quote("{rev}") -> "2"))
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1?tag=v1.0.1", headersUser)).mapJson {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeField("_createdAt").removeField("_updatedAt") shouldEqual exptectedTag1
      }

      val exptectedTag2 = jsonContentOf("/kg/resources/simple-resource-response.json",
                                        Map(quote("{priority}") -> "5", quote("{rev}") -> "1"))
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1?tag=v1.0.0", headersUser)).mapJson {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeField("_createdAt").removeField("_updatedAt") shouldEqual exptectedTag2
      }
    }
  }

  private def kgReqEntity(path: String = "/kg/projects/project.json",
                          name: String = genString(),
                          base: String = s"${config.kg.uri.toString()}/resources/${genString()}/"): RequestEntity = {
    val rep = Map(quote("{name}") -> name, quote("{base}") -> base)
    jsonContentOf(path, rep).toEntity
  }

}
