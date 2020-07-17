package ch.epfl.bluebrain.nexus.tests.kg

import java.util.regex.Pattern.quote

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.headers.Authorization
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.http.scaladsl.model.{HttpRequest => Req, _}
import akka.stream.alpakka.sse.scaladsl.EventSource
import akka.stream.scaladsl.Sink
import ch.epfl.bluebrain.nexus.rdf.syntax._
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.EitherValues
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import ch.epfl.bluebrain.nexus.tests.iam.types.AclListing
import com.fasterxml.uuid.Generators
import io.circe.Json
import io.circe.parser._
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, Inspectors, OptionValues}

import scala.concurrent.Future
import scala.concurrent.duration._

class EventsSpec
    extends BaseSpec
    with Eventually
    with Inspectors
    with CancelAfterFailure
    with OptionValues
    with EitherValues {

  private val http = Http()

  val orgId     = genId()
  val orgId2    = genId()
  val projId    = genId()
  val id        = s"$orgId/$projId"
  val id2       = s"$orgId2/$projId"
  val timestamp = Generators.timeBasedGenerator().generate()

  "creating projects" should {

    "add necessary permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "organizations/create\",\"events/read")
      ).toEntity
      cl(Req(GET, s"$iamBase/acls/", headersServiceAccount)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        val rev = acls._results.head._rev

        cl(Req(PATCH, s"$iamBase/acls/?rev=$rev", headersServiceAccount, json))
          .mapResp(_.status shouldEqual StatusCodes.OK)
      }
    }

    "succeed creating project 1 if payload is correct" in {
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersJsonUser, orgReqEntity(orgId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PUT, s"$adminBase/projects/$id", headersJsonUser, kgProjectReqEntity(name = id)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "succeed creating project 2 if payload is correct" in {
      cl(Req(PUT, s"$adminBase/orgs/$orgId2", headersJsonUser, orgReqEntity(orgId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PUT, s"$adminBase/projects/$id2", headersJsonUser, kgProjectReqEntity(name = id)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "wait for default project events" in {
      eventually {
        val endpoints = List(
          s"$kgBase/views/$id/nxv:defaultElasticSearchIndex",
          s"$kgBase/views/$id/nxv:defaultSparqlIndex",
          s"$kgBase/resolvers/$id/nxv:defaultInProject",
          s"$kgBase/storages/$id/nxv:diskStorageDefault",
          s"$kgBase/views/$id2/nxv:defaultElasticSearchIndex",
          s"$kgBase/views/$id2/nxv:defaultSparqlIndex",
          s"$kgBase/resolvers/$id2/nxv:defaultInProject",
          s"$kgBase/storages/$id2/nxv:diskStorageDefault"
        )
        forAll(endpoints) { endopoint =>
          cl(Req(GET, endopoint, headersJsonUser))
            .mapResp(_.status shouldEqual StatusCodes.OK)
        }
      }
    }

    "wait for storages to be indexed" in {
      val endpoints = List(
        s"$kgBase/storages/$id",
        s"$kgBase/storages/$id2"
      )

      forAll(endpoints) { endpoint =>
        eventually {
          cl(Req(GET, endpoint, headersJsonUser))
            .mapJson { (json, result) =>
              result.status shouldEqual StatusCodes.OK
              json.hcursor.downField("_total").as[Int].rightValue shouldEqual 1
            }
        }
      }
    }
  }

  "fetching events" should {

    "add events to project" in {

      //Created event
      val payload = jsonContentOf(
        "/kg/resources/simple-resource.json",
        Map(quote("{priority}") -> "3", quote("{resourceId}") -> "1")
      )

      cl(Req(PUT, s"$kgBase/resources/$id/_/test-resource:1", headersJsonUser, payload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)

      cl(Req(PUT, s"$kgBase/resources/$id2/_/test-resource:1", headersJsonUser, payload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)

      //Updated event
      val updatePayload = jsonContentOf(
        "/kg/resources/simple-resource.json",
        Map(quote("{priority}") -> "5", quote("{resourceId}") -> "1")
      )
      cl(Req(PUT, s"$kgBase/resources/$id/_/test-resource:1?rev=1", headersJsonUser, updatePayload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.OK)

      //TagAdded event
      val tag1 = jsonContentOf("/kg/resources/tag.json", Map(quote("{tag}") -> "v1.0.0", quote("{rev}") -> "1"))

      cl(Req(POST, s"$kgBase/resources/$id/_/test-resource:1/tags?rev=2", headersJsonUser, tag1.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)

      //Deprecated event
      cl(Req(DELETE, s"$kgBase/resources/$id/_/test-resource:1?rev=3", headersJsonUser, updatePayload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.OK)

      //FileCreated event
      val multipartForm =
        Multipart
          .FormData(
            Multipart.FormData.BodyPart
              .Strict(
                "file",
                HttpEntity(ContentTypes.`application/json`, contentOf("/kg/files/attachment.json")),
                Map("filename" -> "attachment.json")
              )
          )
          .toEntity()

      cl(Req(PUT, s"$kgBase/files/$id/attachment.json", headersJsonUser, multipartForm))
        .mapResp(_.status shouldEqual StatusCodes.Created)

      //FileUpdated event
      val multipartFormUpdate =
        Multipart
          .FormData(
            Multipart.FormData.BodyPart
              .Strict(
                "file",
                HttpEntity(ContentTypes.`application/json`, contentOf("/kg/files/attachment2.json")),
                Map("filename" -> "attachment.json")
              )
          )
          .toEntity()

      cl(Req(PUT, s"$kgBase/files/$id/attachment.json?rev=1", headersJsonUser, multipartFormUpdate))
        .mapResp(_.status shouldEqual StatusCodes.OK)

    }

    "fetch resource events filtered by project" in {

      val projectUuid = cl(Req(GET, s"$adminBase/projects/$id", headersJsonUser)).jsonValue.asObject
        .value("_uuid")
        .value
        .asString
        .value

      val organizationUuid = cl(Req(GET, s"$adminBase/orgs/$orgId", headersJsonUser)).jsonValue.asObject
        .value("_uuid")
        .value
        .asString
        .value

      val projectEvents: Seq[ServerSentEvent] =
        EventSource(s"$kgBase/resources/$id/events", send, initialLastEventId = Some(timestamp.toString))
        //drop resolver, views and storage events
          .drop(4)
          .take(6)
          .runWith(Sink.seq)
          .futureValue

      projectEvents.size shouldEqual 6
      projectEvents
        .map(_.getEventType().get())
        .toList shouldEqual List("Created", "Updated", "TagAdded", "Deprecated", "FileCreated", "FileUpdated")
      val result = Json.arr(projectEvents.map(e => parse(e.getData()).rightValue): _*)

      removeLocation(removeInstants(result)) shouldEqual jsonContentOf(
        "/kg/events/events.json",
        Map(
          quote("{resources}")        -> s"$kgBase/resources/$id",
          quote("{iamBase}")          -> config.iam.uri.toString(),
          quote("{realm}")            -> config.iam.testRealm,
          quote("{user}")             -> config.iam.testUserSub,
          quote("{projectUuid}")      -> projectUuid,
          quote("{organizationUuid}") -> organizationUuid
        )
      )
    }

    "fetch resource events filtered by organization 1" in {

      val projectUuid = cl(Req(GET, s"$adminBase/projects/$id", headersJsonUser)).jsonValue.asObject
        .value("_uuid")
        .value
        .asString
        .value

      val organizationUuid = cl(Req(GET, s"$adminBase/orgs/$orgId", headersJsonUser)).jsonValue.asObject
        .value("_uuid")
        .value
        .asString
        .value

      val projectEvents: Seq[ServerSentEvent] =
        EventSource(s"$kgBase/resources/$orgId/events", send, initialLastEventId = Some(timestamp.toString))
        //drop resolver, views and storage events
          .drop(4)
          .take(6)
          .runWith(Sink.seq)
          .futureValue

      projectEvents.size shouldEqual 6
      projectEvents
        .map(_.getEventType().get())
        .toList shouldEqual List("Created", "Updated", "TagAdded", "Deprecated", "FileCreated", "FileUpdated")
      val result = Json.arr(projectEvents.map(e => parse(e.getData()).rightValue): _*)

      removeLocation(removeInstants(result)) shouldEqual jsonContentOf(
        "/kg/events/events.json",
        Map(
          quote("{resources}")        -> s"$kgBase/resources/$id",
          quote("{iamBase}")          -> config.iam.uri.toString(),
          quote("{realm}")            -> config.iam.testRealm,
          quote("{user}")             -> config.iam.testUserSub,
          quote("{projectUuid}")      -> projectUuid,
          quote("{organizationUuid}") -> organizationUuid
        )
      )
    }

    "fetch resource events filtered by organization 2" in {

      val projectUuid = cl(Req(GET, s"$adminBase/projects/$id2", headersJsonUser)).jsonValue.asObject
        .value("_uuid")
        .value
        .asString
        .value

      val organizationUuid = cl(Req(GET, s"$adminBase/orgs/$orgId2", headersJsonUser)).jsonValue.asObject
        .value("_uuid")
        .value
        .asString
        .value

      val projectEvents: Seq[ServerSentEvent] =
        EventSource(s"$kgBase/resources/$orgId2/events", send, initialLastEventId = Some(timestamp.toString))
        //drop resolver, views and storage events
          .drop(4)
          .takeWithin(3.seconds)
          .runWith(Sink.seq)
          .futureValue

      projectEvents.size shouldEqual 1
      projectEvents.map(_.getEventType().get()).toList shouldEqual List("Created")
      val result = Json.arr(projectEvents.map(e => parse(e.getData()).rightValue): _*)

      removeLocation(removeInstants(result)) shouldEqual jsonContentOf(
        "/kg/events/events2.json",
        Map(
          quote("{resources}")        -> s"$kgBase/resources/$id",
          quote("{iamBase}")          -> config.iam.uri.toString(),
          quote("{realm}")            -> config.iam.testRealm,
          quote("{user}")             -> config.iam.testUserSub,
          quote("{projectUuid}")      -> projectUuid,
          quote("{organizationUuid}") -> organizationUuid
        )
      )
    }

    "fetch global events" in {

      val organizationUuid = cl(Req(GET, s"$adminBase/orgs/$orgId", headersJsonUser)).jsonValue.asObject
        .value("_uuid")
        .value
        .asString
        .value

      val organization2Uuid = cl(Req(GET, s"$adminBase/orgs/$orgId2", headersJsonUser)).jsonValue.asObject
        .value("_uuid")
        .value
        .asString
        .value

      val projectUuid = cl(Req(GET, s"$adminBase/projects/$id", headersJsonUser)).jsonValue.asObject
        .value("_uuid")
        .value
        .asString
        .value

      val project2Uuid = cl(Req(GET, s"$adminBase/projects/$id2", headersJsonUser)).jsonValue.asObject
        .value("_uuid")
        .value
        .asString
        .value

      val events: Seq[ServerSentEvent] =
        EventSource(s"$kgBase/resources/events", send, initialLastEventId = Some(timestamp.toString))
          .drop(8)
          .take(7)
          .runWith(Sink.seq)
          .futureValue

      events.size shouldEqual 7
      events.map(_.getEventType().get()).toList shouldEqual List(
        "Created",
        "Created",
        "Updated",
        "TagAdded",
        "Deprecated",
        "FileCreated",
        "FileUpdated"
      )
      val result = Json.arr(events.map(e => parse(e.getData()).rightValue): _*)
      removeLocation(removeInstants(result)) shouldEqual jsonContentOf(
        "/kg/events/events-multi-project.json",
        Map(
          quote("{resources}")         -> s"$kgBase/resources/$id",
          quote("{iamBase}")           -> config.iam.uri.toString(),
          quote("{realm}")             -> config.iam.testRealm,
          quote("{user}")              -> config.iam.testUserSub,
          quote("{projectUuid}")       -> projectUuid,
          quote("{project2Uuid}")      -> project2Uuid,
          quote("{organizationUuid}")  -> organizationUuid,
          quote("{organization2Uuid}") -> organization2Uuid
        )
      )

    }
  }

  private def removeInstants(json: Json): Json =
    json.hcursor
      .withFocus(
        _.mapArray(
          _.map(
            _.removeKeys("_instant", "_updatedAt")
          )
        )
      )
      .top
      .value

  private def removeLocation(json: Json): Json =
    json.hcursor
      .withFocus(
        _.mapArray(
          _.map { json =>
            json.hcursor.downField("_attributes").focus match {
              case Some(attr) =>
                json.removeKeys("_attributes") deepMerge
                  Json.obj("_attributes" -> attr.removeKeys("_location", "_uuid", "_path"))
              case None => json
            }
          }
        )
      )
      .top
      .value

  private def send(request: Req): Future[HttpResponse] =
    http.singleRequest(request.addHeader(Authorization(credUser))).map { resp =>
      if (!resp.status.isSuccess())
        println(s"HTTP response when performing SSE request: status = '${resp.status}' ; req = ${request.uri}")
      resp
    }
}
