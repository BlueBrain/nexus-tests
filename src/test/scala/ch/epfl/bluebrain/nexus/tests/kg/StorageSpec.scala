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
import ch.epfl.bluebrain.nexus.tests.iam.types.{AclListing, Permissions}
import io.circe.Json
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, EitherValues, Inspectors}
import software.amazon.awssdk.auth.credentials._
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model._

import scala.collection.immutable.Seq
import scala.collection.JavaConverters._

class StorageSpec extends BaseSpec with Eventually with Inspectors with CancelAfterFailure with EitherValues {

  private val orgId  = genId()
  private val projId = genId()
  private val fullId = s"$orgId/$projId"
  private val bucket = genId()

  private val credentialsProvider = (s3Config.accessKey, s3Config.secretKey) match {
    case (Some(ak), Some(sk)) => StaticCredentialsProvider.create(AwsBasicCredentials.create(ak, sk))
    case _                    => AnonymousCredentialsProvider.create()
  }

  private val s3Client = S3Client.builder
    .endpointOverride(s3Config.endpointURI)
    .credentialsProvider(credentialsProvider)
    .region(Region.US_EAST_1)
    .build

  override def beforeAll(): Unit = {
    super.beforeAll()
    val _ = s3Client.createBucket(CreateBucketRequest.builder.bucket(bucket).build)
  }

  override def afterAll(): Unit = {
    val objects = s3Client.listObjects(ListObjectsRequest.builder.bucket(bucket).build)
    objects.contents.asScala.foreach { obj =>
      s3Client.deleteObject(DeleteObjectRequest.builder.bucket(bucket).key(obj.key).build)
    }
    s3Client.deleteBucket(DeleteBucketRequest.builder.bucket(bucket).build)
    super.afterAll()
  }

  "creating projects" should {

    "add necessary permissions for custom storage" in {
      cl(Req(GET, s"$iamBase/permissions", headersGroup)).mapDecoded[Permissions] { (permissions, result) =>
        result.status shouldEqual StatusCodes.OK
        if (permissions.permissions.contains("some-read") && permissions.permissions.contains("some-write"))
          succeed
        else {
          val body = jsonContentOf("/iam/permissions/append.json",
                                   Map(
                                     quote("{perms}") -> List("some-read", "some-write").mkString("\",\"")
                                   )).toEntity
          cl(Req(PATCH, s"$iamBase/permissions?rev=${permissions._rev}", headersGroup, body))
            .mapResp(_.status shouldEqual StatusCodes.OK)
        }
      }
    }

    "add necessary ACLs for user" in {
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

    "succeed creating a DiskStorage" in {
      val payload = jsonContentOf("/kg/storages/disk.json")
      eventually {
        cl(Req(POST, s"$kgBase/storages/$fullId", headersUserAcceptJson, payload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }

      cl(Req(GET, s"$kgBase/storages/$fullId/nxv:mystorage", headersUserAcceptJson))
        .mapJson { (json, result) =>
          val expected = jsonContentOf(
            "/kg/storages/disk-response.json",
            Map(
              quote("{kgBase}")  -> s"$kgBase",
              quote("{id}")      -> "nxv:mystorage",
              quote("{project}") -> fullId,
              quote("{read}")    -> "resources/read",
              quote("{write}")   -> "files/write",
              quote("{iamBase}") -> config.iam.uri.toString(),
              quote("{user}")    -> config.iam.userSub
            )
          )
          json.removeFields("_createdAt", "_updatedAt") should equalIgnoreArrayOrder(expected)
          result.status shouldEqual StatusCodes.OK
        }

      cl(Req(GET, s"$iamBase/permissions", headersGroup)).mapDecoded[Permissions] { (permissions, result) =>
        result.status shouldEqual StatusCodes.OK
        if (!Set("disk/read", "disk/write").subsetOf(permissions.permissions)) {
          val body = jsonContentOf("/iam/permissions/append.json",
                                   Map(
                                     quote("{perms}") -> """disk/read", "disk/write"""
                                   )).toEntity
          cl(Req(PATCH, s"$iamBase/permissions?rev=${permissions._rev}", headersGroup, body))
            .mapResp(_.status shouldEqual StatusCodes.OK)
        } else {
          succeed
        }
      }

      val payload2 = jsonContentOf("/kg/storages/disk-perms.json")
      eventually {
        cl(Req(POST, s"$kgBase/storages/$fullId", headersUserAcceptJson, payload2.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }

      cl(Req(GET, s"$kgBase/storages/$fullId/nxv:mystorage2", headersUserAcceptJson))
        .mapJson { (json, result) =>
          val expected = jsonContentOf(
            "/kg/storages/disk-response.json",
            Map(
              quote("{kgBase}")  -> s"$kgBase",
              quote("{id}")      -> "nxv:mystorage2",
              quote("{project}") -> fullId,
              quote("{read}")    -> "disk/read",
              quote("{write}")   -> "disk/write",
              quote("{iamBase}") -> config.iam.uri.toString(),
              quote("{user}")    -> config.iam.userSub
            )
          )
          json.removeFields("_createdAt", "_updatedAt") should equalIgnoreArrayOrder(expected)
          result.status shouldEqual StatusCodes.OK
        }
    }

    "succeed creating an S3Storage" in {
      val payload = jsonContentOf(
        "/kg/storages/s3.json",
        Map(
          quote("{storageId}") -> "https://bluebrain.github.io/nexus/vocabulary/mys3storage",
          quote("{bucket}")    -> bucket,
          quote("{endpoint}")  -> s3Config.endpoint.toString,
          quote("{accessKey}") -> s3Config.accessKey.get,
          quote("{secretKey}") -> s3Config.secretKey.get
        )
      )

      eventually {
        cl(Req(POST, s"$kgBase/storages/$fullId", headersUserAcceptJson, payload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }

      cl(Req(GET, s"$iamBase/permissions", headersGroup)).mapDecoded[Permissions] { (permissions, result) =>
        result.status shouldEqual StatusCodes.OK
        if (!Set("s3/read", "s3/write").subsetOf(permissions.permissions)) {
          val body = jsonContentOf("/iam/permissions/append.json",
                                   Map(
                                     quote("{perms}") -> """s3/read", "s3/write"""
                                   )).toEntity
          cl(Req(PATCH, s"$iamBase/permissions?rev=${permissions._rev}", headersGroup, body))
            .mapResp(_.status shouldEqual StatusCodes.OK)
        } else {
          succeed
        }
      }

      cl(Req(GET, s"$kgBase/storages/$fullId/nxv:mys3storage", headersUserAcceptJson))
        .mapJson { (json, result) =>
          val expected = jsonContentOf(
            "/kg/storages/s3-response.json",
            Map(
              quote("{kgBase}")  -> s"$kgBase",
              quote("{id}")      -> "nxv:mys3storage",
              quote("{project}") -> fullId,
              quote("{bucket}")  -> bucket,
              quote("{read}")    -> "resources/read",
              quote("{write}")   -> "files/write",
              quote("{iamBase}") -> config.iam.uri.toString(),
              quote("{user}")    -> config.iam.userSub
            )
          )
          json.removeFields("_createdAt", "_updatedAt") should equalIgnoreArrayOrder(expected)
          result.status shouldEqual StatusCodes.OK
        }

      val payload2 = jsonContentOf(
        "/kg/storages/s3.json",
        Map(
          quote("{storageId}") -> "https://bluebrain.github.io/nexus/vocabulary/mys3storage2",
          quote("{bucket}")    -> bucket,
          quote("{endpoint}")  -> s3Config.endpoint.toString,
          quote("{accessKey}") -> s3Config.accessKey.get,
          quote("{secretKey}") -> s3Config.secretKey.get
        )
      ) deepMerge Json.obj(
        "region"          -> Json.fromString("not-important"),
        "readPermission"  -> Json.fromString("s3/read"),
        "writePermission" -> Json.fromString("s3/write")
      )

      eventually {
        cl(Req(POST, s"$kgBase/storages/$fullId", headersUserAcceptJson, payload2.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }

      cl(Req(GET, s"$kgBase/storages/$fullId/nxv:mys3storage2", headersUserAcceptJson))
        .mapJson { (json, result) =>
          val expected = jsonContentOf(
            "/kg/storages/s3-response.json",
            Map(
              quote("{kgBase}")  -> s"$kgBase",
              quote("{id}")      -> "nxv:mys3storage2",
              quote("{project}") -> fullId,
              quote("{bucket}")  -> bucket,
              quote("{read}")    -> "s3/read",
              quote("{write}")   -> "s3/write",
              quote("{iamBase}") -> config.iam.uri.toString(),
              quote("{user}")    -> config.iam.userSub
            )
          ).deepMerge(Json.obj("region" -> Json.fromString("not-important")))

          json.removeFields("_createdAt", "_updatedAt") should equalIgnoreArrayOrder(expected)
          result.status shouldEqual StatusCodes.OK
        }
    }

    "fail creating a DiskStorage on a wrong volume" in {
      val volume  = "/" + genString()
      val payload = jsonContentOf("/kg/storages/disk.json") deepMerge Json.obj("volume" -> Json.fromString(volume))

      eventually {
        cl(Req(POST, s"$kgBase/storages/$fullId", headersUserAcceptJson, payload.toEntity))
          .mapJson { (json, result) =>
            json shouldEqual jsonContentOf("/kg/storages/error.json", Map(quote("{volume}") -> volume))
            result.status shouldEqual StatusCodes.BadRequest
          }
      }
    }

    "fail creating an S3Storage with an invalid bucket" in {
      val payload = jsonContentOf(
        "/kg/storages/s3.json",
        Map(
          quote("{storageId}") -> "https://bluebrain.github.io/nexus/vocabulary/mys3storage",
          quote("{bucket}")    -> "foobar",
          quote("{endpoint}")  -> s3Config.endpoint.toString,
          quote("{accessKey}") -> s3Config.accessKey.get,
          quote("{secretKey}") -> s3Config.secretKey.get
        )
      )

      eventually {
        cl(Req(POST, s"$kgBase/storages/$fullId", headersUserAcceptJson, payload.toEntity))
          .mapJson { (json, result) =>
            json shouldEqual jsonContentOf("/kg/storages/s3-error.json")
            result.status shouldEqual StatusCodes.BadRequest
          }
      }
    }
  }

  "uploading an attachment against the S3 storage" should {

    "upload attachment with JSON" in eventually {
      val entity = HttpEntity(ContentTypes.`application/json`, contentOf("/kg/resources/attachment.json"))
      val multipartForm =
        FormData(BodyPart.Strict("file", entity, Map("filename" -> "s3attachment.json"))).toEntity()

      cl(
        Req(PUT,
            s"$kgBase/files/$fullId/s3attachment.json?storage=nxv:mys3storage",
            headersUserAcceptJson,
            multipartForm))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "fetch attachment" in {
      val expectedContent = contentOf("/kg/resources/attachment.json")
      cl(Req(GET, s"$kgBase/files/$fullId/attachment:s3attachment.json", headersUser ++ Seq(Accept(MediaRanges.`*/*`))))
        .mapString { (content, result) =>
          result.status shouldEqual StatusCodes.OK
          result.header[`Content-Disposition`].value.dispositionType shouldEqual ContentDispositionTypes.attachment
          result.header[`Content-Disposition`].value.params.get("filename").value shouldEqual "UTF-8''s3attachment.json"
          result.header[`Content-Type`].value.value shouldEqual "application/json"
          content shouldEqual expectedContent
        }
    }

    "fetch gzipped attachment" in {
      val expectedContent = contentOf("/kg/resources/attachment.json")
      val requestHeaders  = headersUser ++ Seq(Accept(MediaRanges.`*/*`), `Accept-Encoding`(HttpEncodings.gzip))
      cl(Req(GET, s"$kgBase/files/$fullId/attachment:s3attachment.json", requestHeaders))
        .mapByteString { (content, result) =>
          result.status shouldEqual StatusCodes.OK
          result.header[`Content-Encoding`].value.encodings shouldEqual Seq(HttpEncodings.gzip)
          result.header[`Content-Disposition`].value.dispositionType shouldEqual ContentDispositionTypes.attachment
          result.header[`Content-Disposition`].value.params.get("filename").value shouldEqual "UTF-8''s3attachment.json"
          result.header[`Content-Type`].value.value shouldEqual "application/json"
          Gzip.decode(content).map(_.decodeString("UTF-8")).futureValue shouldEqual expectedContent
        }
    }

    "update attachment with JSON" in {
      val entity = HttpEntity(ContentTypes.`application/json`, contentOf("/kg/resources/attachment2.json"))
      val multipartForm =
        FormData(BodyPart.Strict("file", entity, Map("filename" -> "s3attachment.json"))).toEntity()

      cl(
        Req(PUT,
            s"$kgBase/files/$fullId/s3attachment.json?storage=nxv:mys3storage&rev=1",
            headersUserAcceptJson,
            multipartForm))
        .mapResp(_.status shouldEqual StatusCodes.OK)
    }

    "fetch updated attachment" in {

      val expectedContent = contentOf("/kg/resources/attachment2.json")
      cl(Req(GET, s"$kgBase/files/$fullId/attachment:s3attachment.json", headersUser ++ Seq(Accept(MediaRanges.`*/*`))))
        .mapString { (content, result) =>
          result.status shouldEqual StatusCodes.OK
          result.header[`Content-Disposition`].value.dispositionType shouldEqual ContentDispositionTypes.attachment
          result.header[`Content-Disposition`].value.params.get("filename").value shouldEqual "UTF-8''s3attachment.json"
          result.header[`Content-Type`].value.value shouldEqual "application/json"
          content shouldEqual expectedContent
        }
    }

    "fetch previous revision of attachment" in {

      val expectedContent = contentOf("/kg/resources/attachment.json")
      cl(
        Req(GET,
            s"$kgBase/files/$fullId/attachment:s3attachment.json?rev=1",
            headersUser ++ Seq(Accept(MediaRanges.`*/*`))))
        .mapString { (content, result) =>
          result.status shouldEqual StatusCodes.OK
          result.header[`Content-Disposition`].value.dispositionType shouldEqual ContentDispositionTypes.attachment
          result.header[`Content-Disposition`].value.params.get("filename").value shouldEqual "UTF-8''s3attachment.json"
          result.header[`Content-Type`].value.value shouldEqual "application/json"
          content shouldEqual expectedContent
        }
    }

    "upload second attachment to created storage" in eventually {
      val entity = HttpEntity(ContentTypes.NoContentType, contentOf("/kg/resources/attachment2").getBytes)
      val multipartForm =
        FormData(BodyPart.Strict("file", entity, Map("filename" -> "s3attachment2"))).toEntity()

      cl(
        Req(PUT, s"$kgBase/files/$fullId/s3attachment2?storage=nxv:mys3storage", headersUserAcceptJson, multipartForm)
          .removeHeader("Content-Type"))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "fetch second attachment" in {

      val expectedContent = contentOf("/kg/resources/attachment2")
      cl(Req(GET, s"$kgBase/files/$fullId/attachment:s3attachment2", headersUser ++ Seq(Accept(MediaRanges.`*/*`))))
        .mapString { (content, result) =>
          result.status shouldEqual StatusCodes.OK
          result.header[`Content-Disposition`].value.dispositionType shouldEqual ContentDispositionTypes.attachment
          result.header[`Content-Disposition`].value.params.get("filename").value shouldEqual "UTF-8''s3attachment2"
          content shouldEqual expectedContent
        }
    }

    "delete the attachment" in {
      cl(Req(DELETE, s"$kgBase/files/$fullId/attachment:s3attachment.json?rev=2", headersUserAcceptJson))
        .mapResp(_.status shouldEqual StatusCodes.OK)
    }

    "fetch attachment metadata" in {

      val expected = jsonContentOf(
        "/kg/resources/attachment-metadata.json",
        Map(
          quote("{filename}")  -> "s3attachment.json",
          quote("{kgBase}")    -> s"$kgBase",
          quote("{storageId}") -> "nxv:mys3storage",
          quote("{projId}")    -> s"$fullId",
          quote("{project}")   -> s"$adminBase/projects/$fullId",
          quote("{iamBase}")   -> config.iam.uri.toString(),
          quote("{realm}")     -> config.iam.testRealm,
          quote("{user}")      -> config.iam.userSub
        )
      )
      val requestHeaders = headersUserAcceptJson
      cl(Req(GET, s"$kgBase/files/$fullId/attachment:s3attachment.json", requestHeaders))
        .mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.hcursor.get[String]("_location").right.value should startWith("http://minio.dev.nexus.ocp.bbp.epfl.ch")
          json.removeFields("_createdAt", "_updatedAt", "_location") shouldEqual expected
        }
    }

    "fail to upload file against a storage with custom permissions" in eventually {
      val entity = HttpEntity(ContentTypes.NoContentType, contentOf("/kg/resources/attachment2").getBytes)
      val multipartForm =
        FormData(BodyPart.Strict("file", entity, Map("filename" -> "s3attachment2"))).toEntity()

      cl(
        Req(PUT, s"$kgBase/files/$fullId/s3attachment3?storage=nxv:mys3storage2", headersUserAcceptJson, multipartForm)
          .removeHeader("Content-Type"))
        .mapResp(_.status shouldEqual StatusCodes.Forbidden)
    }

    "add ACLs for custom storage" in {
      forAll(List("s3/read", "s3/write")) { perm =>
        val json = jsonContentOf(
          "/iam/add.json",
          replSub + (quote("{perms}") -> perm)
        ).toEntity
        cl(Req(GET, s"$iamBase/acls/", headersGroup)).mapDecoded[AclListing] { (acls, result) =>
          result.status shouldEqual StatusCodes.OK
          val rev = acls._results.head._rev
          cl(Req(PATCH, s"$iamBase/acls/?rev=$rev", headersGroup, json)).mapResp(_.status shouldEqual StatusCodes.OK)
        }
      }
    }

    "upload file against a storage with custom permissions" in eventually {
      val entity = HttpEntity(ContentTypes.NoContentType, contentOf("/kg/resources/attachment2").getBytes)
      val multipartForm =
        FormData(BodyPart.Strict("file", entity, Map("filename" -> "s3attachment2"))).toEntity()

      cl(
        Req(PUT, s"$kgBase/files/$fullId/attachment3?storage=nxv:mys3storage2", headersUserAcceptJson, multipartForm)
          .removeHeader("Content-Type"))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }
  }

  "uploading an attachment against the default storage" should {

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

    "upload second attachment to created storage" in eventually {
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
          quote("{filename}")  -> "attachment.json",
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

    "fail to upload file against a storage with custom permissions" in eventually {
      val entity = HttpEntity(ContentTypes.NoContentType, contentOf("/kg/resources/attachment2").getBytes)
      val multipartForm =
        FormData(BodyPart.Strict("file", entity, Map("filename" -> "attachment2"))).toEntity()

      cl(
        Req(PUT, s"$kgBase/files/$fullId/attachment3?storage=nxv:mystorage2", headersUserAcceptJson, multipartForm)
          .removeHeader("Content-Type"))
        .mapResp(_.status shouldEqual StatusCodes.Forbidden)
    }

    "add ACLs for custom storage" in {
      forAll(List("disk/read", "disk/write")) { perm =>
        val json = jsonContentOf(
          "/iam/add.json",
          replSub + (quote("{perms}") -> perm)
        ).toEntity
        cl(Req(GET, s"$iamBase/acls/", headersGroup)).mapDecoded[AclListing] { (acls, result) =>
          result.status shouldEqual StatusCodes.OK
          val rev = acls._results.head._rev
          cl(Req(PATCH, s"$iamBase/acls/?rev=$rev", headersGroup, json)).mapResp(_.status shouldEqual StatusCodes.OK)
        }
      }

    }

    "upload file against a storage with custom permissions" in eventually {
      val entity = HttpEntity(ContentTypes.NoContentType, contentOf("/kg/resources/attachment2").getBytes)
      val multipartForm =
        FormData(BodyPart.Strict("file", entity, Map("filename" -> "attachment2"))).toEntity()

      cl(
        Req(PUT, s"$kgBase/files/$fullId/attachment4?storage=nxv:mystorage2", headersUserAcceptJson, multipartForm)
          .removeHeader("Content-Type"))
        .mapResp(_.status shouldEqual StatusCodes.Created)
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
