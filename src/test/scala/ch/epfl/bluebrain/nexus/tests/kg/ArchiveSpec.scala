package ch.epfl.bluebrain.nexus.tests.kg

import java.io.ByteArrayInputStream
import java.nio.file.{Path, Paths}
import java.security.MessageDigest
import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.Multipart.FormData
import akka.http.scaladsl.model.Multipart.FormData.BodyPart
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.model.{HttpEntity, MediaRanges, MediaTypes, StatusCodes, HttpRequest => Req}
import akka.stream.scaladsl.{Sink, StreamConverters}
import akka.util.ByteString
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import io.circe.Printer
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, EitherValues, Inspectors}

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.concurrent.Future

class ArchiveSpec extends BaseSpec with Eventually with Inspectors with CancelAfterFailure with EitherValues {
  private val orgId          = genId()
  private val projId         = genId()
  private val projId2        = genId()
  private val fullId         = s"$orgId/$projId"
  private val fullId2        = s"$orgId/$projId2"
  private val defaultPrinter = Printer.spaces2.copy(dropNullValues = true)

  private val payload1 =
    jsonContentOf("/kg/resources/simple-resource.json", Map(quote("{priority}") -> "5", quote("{resourceId}") -> "1"))

  private val payload2 =
    jsonContentOf("/kg/resources/simple-resource.json", Map(quote("{priority}") -> "6", quote("{resourceId}") -> "2"))

  private val nexusLogoDigest =
    "edd70eff895cde1e36eaedd22ed8e9c870bb04155d05d275f970f4f255488e993a32a7c914ee195f6893d43b8be4e0b00db0a6d545a8462491eae788f664ea6b"
  private val payload1Digest = digest(ByteString(payload1.printWith(defaultPrinter)))
  private val payload2Digest = digest(ByteString(payload2.printWith(defaultPrinter)))

  private type PathAndContent = (Path, ByteString)

  @tailrec
  private def readEntries(tar: TarArchiveInputStream, entries: List[PathAndContent] = Nil): List[PathAndContent] = {
    val entry = tar.getNextTarEntry
    if (entry == null) entries
    else {
      val data = Array.ofDim[Byte](entry.getSize.toInt)
      tar.read(data)
      readEntries(tar, (Paths.get(entry.getName) -> ByteString(data)) :: entries)
    }
  }

  def digest(byteString: ByteString): String = {
    val digest = MessageDigest.getInstance("SHA-512")
    digest.update(byteString.asByteBuffer)
    digest.digest().map("%02x".format(_)).mkString
  }

  val sinkDigest: Sink[ByteString, Future[String]] =
    Sink
      .fold(MessageDigest.getInstance("SHA-512")) { (digest, currentBytes: ByteString) =>
        digest.update(currentBytes.asByteBuffer)
        digest
      }
      .mapMaterializedValue(_.map(_.digest().map("%02x".format(_)).mkString))

  "creating projects" should {

    "created org using service account" in {
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersServiceAccount, orgReqEntity(orgId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "add necessary ACLs for user" in {
      val json = jsonContentOf("/iam/add.json", replSub + (quote("{perms}") -> "projects/create")).toEntity
      cl(Req(PATCH, s"$iamBase/acls/$orgId?rev=1", headersServiceAccount, json))
        .mapResp(_.status shouldEqual StatusCodes.OK)
    }

    "succeed if payload is correct" in {
      cl(Req(PUT, s"$adminBase/projects/$fullId", headersJsonUser, kgProjectReqEntity(name = fullId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)

      cl(Req(PUT, s"$adminBase/projects/$fullId2", headersJsonUser, kgProjectReqEntity(name = fullId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }
  }

  "creating resources" in {

    val source        = StreamConverters.fromInputStream(() => getClass.getResourceAsStream("/kg/files/nexus-logo.png"))
    val entity        = HttpEntity.IndefiniteLength(MediaTypes.`image/png`, source)
    val multipartForm = FormData(BodyPart("file", entity, Map("filename" -> "nexus-logo.png"))).toEntity()

    eventually {
      cl(Req(PUT, s"$kgBase/files/$fullId/test-resource:logo", headersJsonUser, multipartForm))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    cl(Req(PUT, s"$kgBase/resources/$fullId/_/test-resource:1", headersJsonUser, payload1.toEntity))
      .mapResp(_.status shouldEqual StatusCodes.Created)

    cl(Req(PUT, s"$kgBase/resources/$fullId2/_/test-resource:2", headersJsonUser, payload2.toEntity))
      .mapResp(_.status shouldEqual StatusCodes.Created)
  }

  "creating archives" should {

    "succeed" in {
      val payload = jsonContentOf("/kg/archives/archive3.json", Map(quote("{project2}") -> fullId2))

      cl(Req(PUT, s"$kgBase/archives/$fullId/test-resource:archive", headersJsonUser, payload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "failed if payload is wrong" in {
      val payload = jsonContentOf("/kg/archives/archive-wrong.json")

      cl(Req(PUT, s"$kgBase/archives/$fullId/archive2", headersJsonUser, payload.toEntity))
        .mapJson { (json, resp) =>
          val response = jsonContentOf("/kg/archives/archive-wrong-response.json")
          resp.status shouldEqual StatusCodes.BadRequest
          json.removeField("report") shouldEqual response
        }
    }

    "failed on wrong path" in {
      val wrong    = List.tabulate(2)(i => jsonContentOf(s"/kg/archives/archive-wrong-path${i + 1}.json"))
      val expected = jsonContentOf("/kg/archives/archive-path-invalid.json")
      forAll(wrong) { payload =>
        cl(Req(PUT, s"$kgBase/archives/$fullId/archive2", headersJsonUser, payload.toEntity))
          .mapJson { (json, resp) =>
            json shouldEqual expected
            resp.status shouldEqual StatusCodes.BadRequest
          }
      }
    }

    "failed on path collisions" in {
      val collisions = List.tabulate(2)(i => jsonContentOf(s"/kg/archives/archive-path-collision${i + 1}.json"))
      val expected = jsonContentOf(
        "/kg/archives/archive-path-dup.json",
        Map(quote("{project}") -> fullId, quote("{kg}") -> config.kg.uri.toString())
      )
      forAll(collisions) { payload =>
        cl(Req(PUT, s"$kgBase/archives/$fullId/archive2", headersJsonUser, payload.toEntity))
          .mapJson { (json, resp) =>
            json shouldEqual expected
            resp.status shouldEqual StatusCodes.BadRequest
          }
      }
    }
  }

  "fetching archive" should {

    "succeed returning metadata" in {
      cl(Req(GET, s"$kgBase/archives/$fullId/test-resource:archive", headersJsonUser))
        .mapJson { (json, result) =>
          val resp = jsonContentOf(
            "/kg/archives/archive-response.json",
            Map(
              quote("{project2}") -> fullId2,
              quote("{project1}") -> fullId,
              quote("{kg}")       -> config.kg.uri.toString(),
              quote("{iam}")      -> config.iam.uri.toString(),
              quote("{admin}")    -> config.admin.uri.toString()
            )
          )
          json.removeFields("_createdAt", "_updatedAt") should equalIgnoreArrayOrder(resp)
          result.status shouldEqual StatusCodes.OK
        }
    }

    "succeed returning binary" in {
      val prefix = "https:%2F%2Fdev.nexus.test.com%2Fsimplified-resource%2F"
      cl(Req(GET, s"$kgBase/archives/$fullId/test-resource:archive", headersUser ++ Seq(Accept(MediaRanges.`*/*`))))
        .mapByteString { (byteString, result) =>
          result.entity.contentType shouldEqual MediaTypes.`application/x-tar`.toContentType
          result.status shouldEqual StatusCodes.OK

          val bytes  = new ByteArrayInputStream(byteString.toArray)
          val tar    = new TarArchiveInputStream(bytes)
          val unpack = readEntries(tar).map { case (path, content) => path.toString -> digest(content) }.toMap
          unpack(s"$fullId/${prefix}1.json") shouldEqual payload1Digest
          unpack(s"$fullId2/${prefix}2.json") shouldEqual payload2Digest
          unpack("some/other/nexus-logo.png") shouldEqual nexusLogoDigest
        }
    }

    "delete resources/read permissions for user on project 2" in {
      val json =
        jsonContentOf("/iam/subtract-permissions.json", replSub + (quote("{perms}") -> "resources/read")).toEntity
      cl(Req(PATCH, s"$iamBase/acls/$fullId2?rev=1", headersServiceAccount, json))
        .mapResp(_.status shouldEqual StatusCodes.OK)
    }

    "failed when a resource in the archive cannot be fetched" in {
      cl(Req(GET, s"$kgBase/archives/$fullId/test-resource:archive", headersUser ++ Seq(Accept(MediaRanges.`*/*`))))
        .mapJson { (json, result) =>
          json shouldEqual jsonContentOf("/kg/archives/archive-element-not-found.json")
          result.status shouldEqual StatusCodes.NotFound
        }
    }
    "succeed returning metadata using query param ignoreNotFound" in {
      cl(
        Req(
          GET,
          s"$kgBase/archives/$fullId/test-resource:archive?ignoreNotFound=true",
          headersUser ++ Seq(Accept(MediaRanges.`*/*`))
        )
      ).mapResp { result =>
        result.entity.contentType shouldEqual MediaTypes.`application/x-tar`.toContentType
        result.status shouldEqual StatusCodes.OK
      }
    }
  }
}
