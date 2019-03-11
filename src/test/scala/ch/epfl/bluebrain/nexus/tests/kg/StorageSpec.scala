package ch.epfl.bluebrain.nexus.tests.kg

import java.util.regex.Pattern.quote

import akka.http.scaladsl.coding.Gzip
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.Multipart.FormData
import akka.http.scaladsl.model.Multipart.FormData.BodyPart
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, MediaRanges, Multipart, StatusCodes, HttpRequest => Req}
import akka.http.scaladsl.unmarshalling.PredefinedFromEntityUnmarshallers.stringUnmarshaller
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import ch.epfl.bluebrain.nexus.tests.iam.types.AclListing
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, Inspectors}

import scala.collection.immutable.Seq

class StorageSpec extends BaseSpec with Eventually with Inspectors with CancelAfterFailure {

  val orgId  = genId()
  val projId = genId()
  val fullId = s"$orgId/$projId"

  "creating projects" should {

    "add necessary permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "organizations/create")
      ).toEntity
      cl(Req(GET, s"$iamBase/acls/", headersGroup)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        val rev = acls._results.head._rev

        cl(Req(PATCH, s"$iamBase/acls/?rev=$rev", headersGroup, json)).mapResp(_.status shouldEqual StatusCodes.OK)
      }
    }

    "succeed if payload is correct" in {
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersUserAcceptJson, orgReqEntity(orgId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PUT, s"$adminBase/projects/$fullId", headersUserAcceptJson, kgProjectReqEntity(name = fullId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }
  }

  "creating a storage" should {

    "create a DiskStorage" in {

      val payload = jsonContentOf("/kg/storages/disk.json")

      eventually {
        cl(Req(POST, s"$kgBase/storages/$fullId", headersUserAcceptJson, payload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }
  }

  "uploading an attachment against a storage" should {

    "upload attachment with JSON" in eventually {
      val entity = HttpEntity(ContentTypes.`application/json`, contentOf("/kg/resources/attachment.json"))
      val multipartForm =
        FormData(BodyPart.Strict("file", entity, Map("filename" -> "attachment.json"))).toEntity()

      cl(Req(PUT, s"$kgBase/files/$fullId/attachment.json", headersUserAcceptJson, multipartForm))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "fetch attachment" in {
      val expectedContent = contentOf("/kg/resources/attachment.json")
      cl(Req(GET, s"$kgBase/files/$fullId/attachment:attachment.json", headersUser ++ Seq(Accept(MediaRanges.`*/*`))))
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
      val requestHeaders  = headersUser ++ Seq(Accept(MediaRanges.`*/*`), `Accept-Encoding`(HttpEncodings.gzip))
      cl(Req(GET, s"$kgBase/files/$fullId/attachment:attachment.json", requestHeaders))
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
      val entity = HttpEntity(ContentTypes.`application/json`, contentOf("/kg/resources/attachment2.json"))
      val multipartForm =
        FormData(BodyPart.Strict("file", entity, Map("filename" -> "attachment.json"))).toEntity()

      cl(
        Req(PUT,
            s"$kgBase/files/$fullId/attachment.json?storage=defaultStorage&rev=1",
            headersUserAcceptJson,
            multipartForm))
        .mapResp(_.status shouldEqual StatusCodes.OK)
    }

    "fetch updated attachment" in {

      val expectedContent = contentOf("/kg/resources/attachment2.json")
      cl(Req(GET, s"$kgBase/files/$fullId/attachment:attachment.json", headersUser ++ Seq(Accept(MediaRanges.`*/*`))))
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
      cl(
        Req(GET,
            s"$kgBase/files/$fullId/attachment:attachment.json?rev=1",
            headersUser ++ Seq(Accept(MediaRanges.`*/*`))))
        .mapString { (content, result) =>
          result.status shouldEqual StatusCodes.OK
          result.header[`Content-Disposition`].value.dispositionType shouldEqual ContentDispositionTypes.attachment
          result.header[`Content-Disposition`].value.params.get("filename").value shouldEqual "UTF-8''attachment.json"
          result.header[`Content-Type`].value.value shouldEqual "application/json"
          content shouldEqual expectedContent
        }
    }

    "upload second attachment to created storage" in {
      val entity = HttpEntity(ContentTypes.NoContentType, contentOf("/kg/resources/attachment2").getBytes)
      val multipartForm =
        FormData(BodyPart.Strict("file", entity, Map("filename" -> "attachment2"))).toEntity()

      cl(
        Req(PUT, s"$kgBase/files/$fullId/attachment2?storage=nxv:mystorage", headersUserAcceptJson, multipartForm)
          .removeHeader("Content-Type"))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "attempt to upload a third attachment against an storage that does not exists" in {
      val entity = HttpEntity(ContentTypes.NoContentType, contentOf("/kg/resources/attachment2").getBytes)
      val multipartForm =
        FormData(Multipart.FormData.BodyPart.Strict("file", entity, Map("filename" -> "attachment3"))).toEntity()

      cl(
        Req(PUT, s"$kgBase/files/$fullId/attachment3?storage=nxv:wrong-id", headersUserAcceptJson, multipartForm)
          .removeHeader("Content-Type"))
        .mapResp(_.status shouldEqual StatusCodes.NotFound)
    }

    "fetch second attachment" in {

      val expectedContent = contentOf("/kg/resources/attachment2")
      cl(Req(GET, s"$kgBase/files/$fullId/attachment:attachment2", headersUser ++ Seq(Accept(MediaRanges.`*/*`))))
        .mapString { (content, result) =>
          result.status shouldEqual StatusCodes.OK
          result.header[`Content-Disposition`].value.dispositionType shouldEqual ContentDispositionTypes.attachment
          result.header[`Content-Disposition`].value.params.get("filename").value shouldEqual "UTF-8''attachment2"
          content shouldEqual expectedContent
        }
    }

    "delete the attachment" in {
      cl(Req(DELETE, s"$kgBase/files/$fullId/attachment:attachment.json?rev=2", headersUserAcceptJson))
        .mapResp(_.status shouldEqual StatusCodes.OK)
    }

    "fetch attachment metadata" in {

      val expected = jsonContentOf(
        "/kg/resources/attachment-metadata.json",
        Map(
          quote("{kgBase}")    -> s"$kgBase",
          quote("{storageId}") -> "nxv:diskStorageDefault",
          quote("{projId}")    -> s"$fullId",
          quote("{project}")   -> s"$adminBase/projects/$fullId",
          quote("{iamBase}")   -> config.iam.uri.toString(),
          quote("{realm}")     -> config.iam.testRealm,
          quote("{user}")      -> config.iam.userSub
        )
      )
      val requestHeaders = headersUserAcceptJson
      cl(Req(GET, s"$kgBase/files/$fullId/attachment:attachment.json", requestHeaders))
        .mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeFields("_createdAt", "_updatedAt") shouldEqual expected
        }
    }
  }

  "deprecating a storage" should {
    "deprecate a DiskStorage" in {
      eventually {
        cl(Req(DELETE, s"$kgBase/storages/$fullId/nxv:mystorage?rev=1", headersUserAcceptJson))
          .mapResp(_.status shouldEqual StatusCodes.OK)
      }
    }

    "reject uploading a new file against the deprecated storage" in {
      val entity = HttpEntity(ContentTypes.NoContentType, new String("").getBytes)
      val multipartForm =
        FormData(Multipart.FormData.BodyPart.Strict("file", entity, Map("filename" -> "attachment3"))).toEntity()
      eventually {
        cl(
          Req(PUT, s"$kgBase/files/$fullId/attachment3?storage=nxv:mystorage", headersUserAcceptJson, multipartForm)
            .removeHeader("Content-Type"))
          .mapResp(_.status shouldEqual StatusCodes.NotFound)
      }
    }

    "fetch second attachment metadata" in {

      val expected = jsonContentOf(
        "/kg/resources/attachment2-metadata.json",
        Map(
          quote("{kgBase}")    -> s"$kgBase",
          quote("{storageId}") -> "nxv:mystorage",
          quote("{projId}")    -> s"$fullId",
          quote("{project}")   -> s"$adminBase/projects/$fullId",
          quote("{iamBase}")   -> config.iam.uri.toString(),
          quote("{realm}")     -> config.iam.testRealm,
          quote("{user}")      -> config.iam.userSub
        )
      )
      val requestHeaders = headersUserAcceptJson
      cl(Req(GET, s"$kgBase/files/$fullId/attachment:attachment2", requestHeaders))
        .mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeFields("_createdAt", "_updatedAt") shouldEqual expected
        }
    }
  }
}
