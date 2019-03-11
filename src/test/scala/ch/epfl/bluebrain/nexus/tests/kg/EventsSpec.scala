package ch.epfl.bluebrain.nexus.tests.kg

import java.util.regex.Pattern.quote

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.headers.Authorization
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.http.scaladsl.model.{HttpRequest => Req, _}
import akka.stream.alpakka.sse.scaladsl.EventSource
import akka.stream.scaladsl.Sink
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import ch.epfl.bluebrain.nexus.tests.iam.types.AclListing
import com.fasterxml.uuid.Generators
import io.circe.Json
import io.circe.parser._
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, EitherValues, Inspectors, OptionValues}

import scala.concurrent.Future

class EventsSpec
    extends BaseSpec
    with Eventually
    with Inspectors
    with CancelAfterFailure
    with OptionValues
    with EitherValues {

  private val http = Http()

  val orgId     = genId()
  val projId    = genId()
  val id        = s"$orgId/$projId"
  val timestamp = Generators.timeBasedGenerator().generate()

  "creating projects" should {

    "add necessary permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "organizations/create\",\"events/read")
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
      cl(Req(PUT, s"$adminBase/projects/$id", headersUserAcceptJson, kgProjectReqEntity(name = id)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "wait for default project events" in {
      eventually {
        cl(Req(GET, s"$kgBase/views/$id/nxv:defaultElasticSearchIndex", headersUserAcceptJson))
          .mapResp(_.status shouldEqual StatusCodes.OK)
        cl(Req(GET, s"$kgBase/views/$id/nxv:defaultSparqlIndex", headersUserAcceptJson))
          .mapResp(_.status shouldEqual StatusCodes.OK)

        cl(Req(GET, s"$kgBase/resolvers/$id/nxv:defaultInProject", headersUserAcceptJson))
          .mapResp(_.status shouldEqual StatusCodes.OK)
      }

    }
  }

  "fetching events" should {

    "add events to project" in {

      //Created event
      val payload = jsonContentOf("/kg/resources/simple-resource.json",
                                  Map(quote("{priority}") -> "3", quote("{resourceId}") -> "1"))

      cl(Req(PUT, s"$kgBase/resources/$id/_/test-resource:1", headersUserAcceptJson, payload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)

      //Updated event
      val updatePayload = jsonContentOf("/kg/resources/simple-resource.json",
                                        Map(quote("{priority}") -> "5", quote("{resourceId}") -> "1"))
      cl(Req(PUT, s"$kgBase/resources/$id/_/test-resource:1?rev=1", headersUserAcceptJson, updatePayload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.OK)

      //TagAdded event
      val tag1 = jsonContentOf("/kg/resources/tag.json", Map(quote("{tag}") -> "v1.0.0", quote("{rev}") -> "1"))

      cl(Req(POST, s"$kgBase/resources/$id/_/test-resource:1/tags?rev=2", headersUserAcceptJson, tag1.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)

      //Deprecated event
      cl(Req(DELETE, s"$kgBase/resources/$id/_/test-resource:1?rev=3", headersUserAcceptJson, updatePayload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.OK)

      //FileCreated event
      val multipartForm =
        Multipart
          .FormData(
            Multipart.FormData.BodyPart
              .Strict("file",
                      HttpEntity(ContentTypes.`application/json`, contentOf("/kg/resources/attachment.json")),
                      Map("filename" -> "attachment.json")))
          .toEntity()

      cl(Req(PUT, s"$kgBase/files/$id/attachment.json", headersUserAcceptJson, multipartForm))
        .mapResp(_.status shouldEqual StatusCodes.Created)

      //FileUpdated event
      val multipartFormUpdate =
        Multipart
          .FormData(
            Multipart.FormData.BodyPart
              .Strict("file",
                      HttpEntity(ContentTypes.`application/json`, contentOf("/kg/resources/attachment2.json")),
                      Map("filename" -> "attachment.json")))
          .toEntity()

      cl(Req(PUT, s"$kgBase/files/$id/attachment.json?rev=1", headersUserAcceptJson, multipartFormUpdate))
        .mapResp(_.status shouldEqual StatusCodes.OK)

    }

    "fetch project events" in {

      val projectUuid = cl(Req(GET, s"$adminBase/projects/$id", headersUserAcceptJson)).jsonValue.asObject
        .value("_uuid")
        .value
        .asString
        .value

      val projectEvents: Seq[ServerSentEvent] =
        EventSource(s"$kgBase/events/$id", send, initialLastEventId = Some(timestamp.toString))
        //drop resolver, views and storage events
          .drop(4)
          .take(6)
          .runWith(Sink.seq)
          .futureValue

      projectEvents.size shouldEqual 6
      projectEvents.map(_.getEventType().get()).toList shouldEqual List("Created",
                                                                        "Updated",
                                                                        "TagAdded",
                                                                        "Deprecated",
                                                                        "FileCreated",
                                                                        "FileUpdated")

      removeInstants(Json.arr(projectEvents.map(e => parse(e.getData()).right.value): _*)) shouldEqual jsonContentOf(
        "/kg/events/events.json",
        Map(
          quote("{resources}")   -> s"$kgBase/resources/$id",
          quote("{iamBase}")     -> config.iam.uri.toString(),
          quote("{realm}")       -> config.iam.testRealm,
          quote("{user}")        -> config.iam.userSub,
          quote("{projectUuid}") -> projectUuid
        )
      )

    }

    "fetch global  events" in {

      val projectUuid = cl(Req(GET, s"$adminBase/projects/$id", headersUserAcceptJson)).jsonValue.asObject
        .value("_uuid")
        .value
        .asString
        .value

      val events: Seq[ServerSentEvent] =
        EventSource(s"$kgBase/events", send, initialLastEventId = Some(timestamp.toString))
          .drop(4)
          .take(6)
          .runWith(Sink.seq)
          .futureValue

      events.size shouldEqual 6
      events.map(_.getEventType().get()).toList shouldEqual List("Created",
                                                                 "Updated",
                                                                 "TagAdded",
                                                                 "Deprecated",
                                                                 "FileCreated",
                                                                 "FileUpdated")
      removeInstants(Json.arr(events.map(e => parse(e.getData()).right.value): _*)) shouldEqual jsonContentOf(
        "/kg/events/events.json",
        Map(
          quote("{resources}")   -> s"$kgBase/resources/$id",
          quote("{iamBase}")     -> config.iam.uri.toString(),
          quote("{realm}")       -> config.iam.testRealm,
          quote("{user}")        -> config.iam.userSub,
          quote("{projectUuid}") -> projectUuid
        )
      )

    }

  }

  private def removeInstants(json: Json): Json =
    json.hcursor
      .withFocus(
        _.mapArray(
          _.map(
            _.removeFields("_instant", "_updatedAt")
          )
        )
      )
      .top
      .value
  private def send(request: Req): Future[HttpResponse] =
    http.singleRequest(request.addHeader(Authorization(credUser))).map { resp =>
      if (!resp.status.isSuccess())
        println(s"HTTP response when performing SSE request: status = '${resp.status}'")
      resp
    }
}
