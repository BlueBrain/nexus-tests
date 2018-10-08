package ch.epfl.bluebrain.nexus.tests.kg

import java.util.regex.Pattern.quote

import akka.http.scaladsl.coding.Gzip
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, Multipart, StatusCodes, HttpRequest => Req}
import akka.http.scaladsl.unmarshalling.PredefinedFromEntityUnmarshallers.stringUnmarshaller
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import io.circe.Json
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, Inspectors}

import scala.collection.immutable.Seq

class ResourcesSpec extends BaseSpec with Eventually with Inspectors with CancelAfterFailure {

  val orgId   = genId()
  val projId1 = genId()
  val projId2 = genId()
  val id1     = s"$orgId/$projId1"
  val id2     = s"$orgId/$projId2"

  "creating projects" should {

    "add necessary permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> """projects/create","projects/read","orgs/write","orgs/read","schemas/manage","resolvers/manage","resources/create","resources/read","resources/write","orgs/create""")
      ).toEntity
      cl(Req(PUT, s"$iamBase/acls/", headersGroup, json)).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
        result.entity.isKnownEmpty() shouldEqual true
      }

      eventually {
        cl(Req(GET, s"$iamBase/acls/", headersUser)).mapJson { (json, result) =>
          json.getArray("acl").head.getArray("permissions").size shouldEqual 10
          result.status shouldEqual StatusCodes.OK
        }
      }
    }

    "succeed if payload is correct" in {
      eventually {
        cl(Req(PUT, s"$adminBase/orgs/$orgId", headersUser, orgReqEntity())).mapResp { result =>
          result.status shouldEqual StatusCodes.Created
        }
      }
      eventually {
        cl(Req(PUT, s"$adminBase/projects/$id1", headersUser, kgProjectReqEntity())).mapResp { result =>
          result.status shouldEqual StatusCodes.Created
        }
      }
      eventually {
        cl(Req(PUT, s"$adminBase/projects/$id2", headersUser, kgProjectReqEntity())).mapResp { result =>
          result.status shouldEqual StatusCodes.Created
        }
      }
    }
  }

  "adding schema" should {
    "create a schema" in {
      val schemaPayload = jsonContentOf("/kg/schemas/simple-schema.json")

      eventually {
        cl(Req(PUT, s"$kgBase/schemas/$id1/test-schema", headersUser, schemaPayload.toEntity)).mapString {
          (_, result) =>
            result.status shouldEqual StatusCodes.Created
        }
      }
    }
  }

  "creating a resource" should {
    "succeed if the payload is correct" in {
      val payload = jsonContentOf("/kg/resources/simple-resource.json",
                                  Map(quote("{priority}") -> "5", quote("{resourceId}") -> "1"))

      eventually {
        cl(Req(PUT, s"$kgBase/resources/$id1/test-schema/test-resource:1", headersUser, payload.toEntity)).mapString {
          (_, result) =>
            result.status shouldEqual StatusCodes.Created
        }
      }
    }

    "fetch the payload" in {
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1", headersUser)).mapJson { (json, result) =>
        val expected = jsonContentOf(
          "/kg/resources/simple-resource-response.json",
          Map(quote("{priority}")  -> "5",
              quote("{rev}")       -> "1",
              quote("{resources}") -> s"$kgBase/resources/$id1",
              quote("{project}")   -> s"$adminBase/projects/$id1")
        )
        result.status shouldEqual StatusCodes.OK
        json.removeField("_createdAt").removeField("_updatedAt") should equalIgnoreArrayOrder(expected)
      }
    }
  }

  "cross-project resolvers" should {
    "fail if the schema doesn't exist in the project" in {
      val payload = jsonContentOf("/kg/resources/simple-resource.json",
                                  Map(quote("{priority}") -> "3", quote("{resourceId}") -> "1"))

      cl(Req(PUT, s"$kgBase/resources/$id2/test-schema/test-resource:1", headersUser, payload.toEntity)).mapResp {
        result =>
          result.status shouldEqual StatusCodes.NotFound
      }
    }

    "create a cross-project-resolver for proj2" in {
      val resolverPayload =
        jsonContentOf("/kg/resources/cross-project-resolver.json", Map(quote("{project}") -> id1))

      eventually {
        cl(Req(POST, s"$kgBase/resolvers/$id2", headersUser, resolverPayload.toEntity)).mapResp { result =>
          result.status shouldEqual StatusCodes.Created
        }
      }
    }

    "wait for the cross-project resolver to be indexed" in {
      eventually {
        cl(Req(GET, s"$kgBase/resolvers/$id2", headersUser)).mapJson { (json, result) =>
          json.asObject.value("_total").value.asNumber.value.toInt.value shouldEqual 2
          result.status shouldEqual StatusCodes.OK
        }
      }
    }

    "resolve schema from the other project" in {
      val payload = jsonContentOf("/kg/resources/simple-resource.json",
                                  Map(quote("{priority}") -> "3", quote("{resourceId}") -> "1"))

      cl(Req(PUT, s"$kgBase/resources/$id2/test-schema/test-resource:1", headersUser, payload.toEntity)).mapResp {
        result =>
          result.status shouldEqual StatusCodes.Created
      }
    }

  }

  "updating a resource" should {
    "send the update" in {
      val payload = jsonContentOf("/kg/resources/simple-resource.json",
                                  Map(quote("{priority}") -> "3", quote("{resourceId}") -> "1"))

      cl(Req(PUT, s"$kgBase/resources/$id1/test-schema/test-resource:1?rev=1", headersUser, payload.toEntity)).mapResp {
        result =>
          result.status shouldEqual StatusCodes.OK
      }
    }
    "fetch the update" in {
      val expected = jsonContentOf(
        "/kg/resources/simple-resource-response.json",
        Map(quote("{priority}")  -> "3",
            quote("{rev}")       -> "2",
            quote("{resources}") -> s"$kgBase/resources/$id1",
            quote("{project}")   -> s"$adminBase/projects/$id1")
      )
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json.removeField("_createdAt").removeField("_updatedAt") should equalIgnoreArrayOrder(expected)
      }
    }

    "fetch previous revision" in {
      val expected = jsonContentOf(
        "/kg/resources/simple-resource-response.json",
        Map(quote("{priority}")  -> "5",
            quote("{rev}")       -> "1",
            quote("{resources}") -> s"$kgBase/resources/$id1",
            quote("{project}")   -> s"$adminBase/projects/$id1")
      )
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1?rev=1", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json.removeField("_createdAt").removeField("_updatedAt") should equalIgnoreArrayOrder(expected)
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

      val expectedTag1 = jsonContentOf(
        "/kg/resources/simple-resource-response.json",
        Map(quote("{priority}")  -> "3",
            quote("{rev}")       -> "2",
            quote("{resources}") -> s"$kgBase/resources/$id1",
            quote("{project}")   -> s"$adminBase/projects/$id1")
      )
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1?tag=v1.0.1", headersUser)).mapJson {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeField("_createdAt").removeField("_updatedAt") should equalIgnoreArrayOrder(expectedTag1)
      }

      val expectedTag2 = jsonContentOf(
        "/kg/resources/simple-resource-response.json",
        Map(quote("{priority}")  -> "5",
            quote("{rev}")       -> "1",
            quote("{resources}") -> s"$kgBase/resources/$id1",
            quote("{project}")   -> s"$adminBase/projects/$id1")
      )
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1?tag=v1.0.0", headersUser)).mapJson {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeField("_createdAt").removeField("_updatedAt") should equalIgnoreArrayOrder(expectedTag2)
      }
    }
  }

  "uploading an attachment" should {

    "upload attachment with JSON" in {
      val multipartForm =
        Multipart
          .FormData(
            Multipart.FormData.BodyPart
              .Strict("file",
                      HttpEntity(ContentTypes.`application/json`, contentOf("/kg/resources/attachment.json")),
                      Map("filename" -> "attachment.json")))
          .toEntity()

      cl(
        Req(PUT,
            s"$kgBase/resources/$id1/test-schema/test-resource:1/attachments/attachment.json?rev=4",
            headersUser,
            multipartForm)).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
      }
    }

    "fetch attachment" in {
      val expectedContent = contentOf("/kg/resources/attachment.json")
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1/attachments/attachment.json", headersUser))
        .mapString { (content, result) =>
          result.status shouldEqual StatusCodes.OK
          result.header[`Content-Disposition`].value.dispositionType shouldEqual ContentDispositionTypes.attachment
          result.header[`Content-Disposition`].value.params.get("filename").value shouldEqual "UTF-8''attachment.json"
          result.header[`Content-Type`].value.value shouldEqual "application/json"
          content shouldEqual expectedContent
        }
    }

    "fetch gzipped attachment" in {
      val expectedContent = contentOf("/kg/resources/attachment.json")
      val requestHeaders  = headersUser ++ Seq(`Accept-Encoding`(HttpEncodings.gzip))
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1/attachments/attachment.json", requestHeaders))
        .mapByteString { (content, result) =>
          result.status shouldEqual StatusCodes.OK
          result.header[`Content-Encoding`].value.encodings shouldEqual Seq(HttpEncodings.gzip)
          result.header[`Content-Disposition`].value.dispositionType shouldEqual ContentDispositionTypes.attachment
          result.header[`Content-Disposition`].value.params.get("filename").value shouldEqual "UTF-8''attachment.json"
          result.header[`Content-Type`].value.value shouldEqual "application/json"
          Gzip.decode(content).map(_.decodeString("UTF-8")).futureValue shouldEqual expectedContent
        }
    }

    "update attachment with JSON" in {
      val multipartForm =
        Multipart
          .FormData(
            Multipart.FormData.BodyPart
              .Strict("file",
                      HttpEntity(ContentTypes.`application/json`, contentOf("/kg/resources/attachment2.json")),
                      Map("filename" -> "attachment.json")))
          .toEntity()

      cl(
        Req(PUT,
            s"$kgBase/resources/$id1/test-schema/test-resource:1/attachments/attachment.json?rev=5",
            headersUser,
            multipartForm)).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
      }
    }

    "fetch updated attachment" in {

      val expectedContent = contentOf("/kg/resources/attachment2.json")
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1/attachments/attachment.json", headersUser))
        .mapString { (content, result) =>
          result.status shouldEqual StatusCodes.OK
          result.header[`Content-Disposition`].value.dispositionType shouldEqual ContentDispositionTypes.attachment
          result.header[`Content-Disposition`].value.params.get("filename").value shouldEqual "UTF-8''attachment.json"
          result.header[`Content-Type`].value.value shouldEqual "application/json"
          content shouldEqual expectedContent
        }

    }

    "fetch previous revision of attachment" in {

      val expectedContent = contentOf("/kg/resources/attachment.json")
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1/attachments/attachment.json?rev=5", headersUser))
        .mapString { (content, result) =>
          result.status shouldEqual StatusCodes.OK
          result.header[`Content-Disposition`].value.dispositionType shouldEqual ContentDispositionTypes.attachment
          result.header[`Content-Disposition`].value.params.get("filename").value shouldEqual "UTF-8''attachment.json"
          result.header[`Content-Type`].value.value shouldEqual "application/json"
          content shouldEqual expectedContent
        }

    }

    "upload second attachment" in {
      val multipartForm =
        Multipart
          .FormData(
            Multipart.FormData.BodyPart
              .Strict("file",
                      HttpEntity(ContentTypes.NoContentType, contentOf("/kg/resources/attachment2").getBytes),
                      Map("filename" -> "attachment.json")))
          .toEntity()

      cl(
        Req(PUT,
            s"$kgBase/resources/$id1/test-schema/test-resource:1/attachments/attachment2?rev=6",
            headersUser,
            multipartForm).removeHeader("Content-Type")).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
      }
    }

    "fetch second attachment" in {

      val expectedContent = contentOf("/kg/resources/attachment2")
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1/attachments/attachment2", headersUser))
        .mapString { (content, result) =>
          result.status shouldEqual StatusCodes.OK
          result.header[`Content-Disposition`].value.dispositionType shouldEqual ContentDispositionTypes.attachment
          result.header[`Content-Disposition`].value.params.get("filename").value shouldEqual "UTF-8''attachment2"
          content shouldEqual expectedContent
        }

    }

    "delete the attachment" in {
      cl(
        Req(DELETE,
            s"$kgBase/resources/$id1/test-schema/test-resource:1/attachments/attachment.json?rev=7",
            headersUser)).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
      }

      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1/attachments/attachment.json", headersUser))
        .mapResp { result =>
          result.status shouldEqual StatusCodes.NotFound
        }
    }

    "fetch the attachment at a previous revision" in {
      val expectedContent = contentOf("/kg/resources/attachment2.json")
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1/attachments/attachment.json?rev=7", headersUser))
        .mapString { (content, result) =>
          result.status shouldEqual StatusCodes.OK
          result.header[`Content-Disposition`].value.dispositionType shouldEqual ContentDispositionTypes.attachment
          result.header[`Content-Disposition`].value.params.get("filename").value shouldEqual "UTF-8''attachment.json"
          result.header[`Content-Type`].value.value shouldEqual "application/json"
          content shouldEqual expectedContent
        }
    }

  }

  "listing resources" should {
    "add more resource to the project" in {

      forAll(2 to 5) { resourceId =>
        val payload = jsonContentOf("/kg/resources/simple-resource.json",
                                    Map(quote("{priority}") -> "3", quote("{resourceId}") -> s"$resourceId"))
        cl(Req(PUT, s"$kgBase/resources/$id1/test-schema/test-resource:$resourceId", headersUser, payload.toEntity))
          .mapResp { result =>
            result.status shouldEqual StatusCodes.Created
          }
      }

    }
    "list the resources" in {
      val expected = jsonContentOf("/kg/listings/response.json",
                                   Map(quote("{resources}") -> s"$kgBase/resources/$id1",
                                       quote("{project}")   -> s"$adminBase/projects/$id1"))
      eventually {
        cl(Req(GET, s"$kgBase/resources/$id1/test-schema", headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          removeSearchMetadata(json) shouldEqual expected
        }
      }
    }
  }

  def removeSearchMetadata(json: Json): Json =
    json.hcursor
      .downField("_results")
      .withFocus(
        _.mapArray(
          _.map(
            _.removeField("_createdAt").removeField("_createdBy").removeField("_updatedAt").removeField("_updatedBy")
          )
        )
      )
      .top
      .value
}
