package ch.epfl.bluebrain.nexus.tests.kg
import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.{StatusCodes, HttpRequest => Req}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.EitherValues
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import io.circe.Json
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, Inspectors}

class CompositeViewsSpec extends BaseSpec with Eventually with Inspectors with CancelAfterFailure with EitherValues {

  val orgId        = genId()
  val bandsProject = s"$orgId/bands"

  val albumsProject = s"$orgId/albums"

  val songsProject = s"$orgId/songs"

  override def afterAll() = {
    val _ = cl(Req(DELETE, s"$kgBase/views/${orgId}/bands/composite?rev=1", headersJsonUser)).mapResp { resp =>
      resp.status shouldEqual StatusCodes.OK
    }
  }

  "creating projects" should {

    "add necessary permissions for user" in {
      val json =
        jsonContentOf(
          "/iam/add.json",
          replSub + (quote("{perms}") -> "organizations/create\",\"views/query\",\"events/read")
        ).toEntity

      cl(Req(PATCH, s"$iamBase/acls/$orgId", headersServiceAccount, json))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "succeed if payload is correct" in {

      val projectPayload = jsonContentOf("/kg/views/composite/project.json").toEntity
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersJsonUser, orgReqEntity(orgId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PUT, s"$adminBase/projects/$bandsProject", headersJsonUser, projectPayload))
        .mapResp(_.status shouldEqual StatusCodes.Created)

      cl(Req(PUT, s"$adminBase/projects/$albumsProject", headersJsonUser, projectPayload))
        .mapResp(_.status shouldEqual StatusCodes.Created)

      cl(Req(PUT, s"$adminBase/projects/$songsProject", headersJsonUser, projectPayload))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "wait until in project resolver is created" in {
      eventually {
        cl(Req(GET, s"$kgBase/resolvers/$bandsProject", headersJsonUser)).mapJson { (json, result) =>
          json.asObject.value("_total").value.asNumber.value.toInt.value shouldEqual 1
          result.status shouldEqual StatusCodes.OK
        }
      }
      eventually {
        cl(Req(GET, s"$kgBase/resolvers/$albumsProject", headersJsonUser)).mapJson { (json, result) =>
          json.asObject.value("_total").value.asNumber.value.toInt.value shouldEqual 1
          result.status shouldEqual StatusCodes.OK
        }
      }

      eventually {
        cl(Req(GET, s"$kgBase/resolvers/$songsProject", headersJsonUser)).mapJson { (json, result) =>
          json.asObject.value("_total").value.asNumber.value.toInt.value shouldEqual 1
          result.status shouldEqual StatusCodes.OK
        }
      }

    }

  }

  "uploading data" should {

    "upload context" in {
      val context = jsonContentOf("/kg/views/composite/context.json")
      cl(Req(POST, s"$kgBase/resources/$songsProject", headersJsonUser, context.toEntity)).mapResp { resp =>
        resp.status shouldEqual StatusCodes.Created
      }
      cl(Req(POST, s"$kgBase/resources/$albumsProject", headersJsonUser, context.toEntity)).mapResp { resp =>
        resp.status shouldEqual StatusCodes.Created
      }
      cl(Req(POST, s"$kgBase/resources/$bandsProject", headersJsonUser, context.toEntity)).mapResp { resp =>
        resp.status shouldEqual StatusCodes.Created
      }
    }

    "upload songs" in {
      jsonContentOf("/kg/views/composite/songs1.json").asArray.value.foreach { song =>
        cl(Req(POST, s"$kgBase/resources/$songsProject", headersJsonUser, song.toEntity)).mapResp { resp =>
          resp.status shouldEqual StatusCodes.Created
        }
      }
    }
    "upload albums" in {
      jsonContentOf("/kg/views/composite/albums.json").asArray.value.foreach { album =>
        cl(Req(POST, s"$kgBase/resources/$albumsProject", headersJsonUser, album.toEntity)).mapResp { resp =>
          resp.status shouldEqual StatusCodes.Created
        }
      }
    }
    "upload bands" in {
      jsonContentOf("/kg/views/composite/bands.json").asArray.value.foreach { band =>
        cl(Req(POST, s"$kgBase/resources/$bandsProject", headersJsonUser, band.toEntity)).mapResp { resp =>
          resp.status shouldEqual StatusCodes.Created
        }
      }
    }
  }

  "creating the view" should {
    "create a composite view" in {

      val view = jsonContentOf(
        "/kg/views/composite/composite-view.json",
        replSub ++ Map(
          quote("{org}")               -> orgId,
          quote("{remoteEndpoint}")    -> kgBase.toString,
          quote("{remoteSourceToken}") -> config.iam.testUserToken
        )
      )
      cl(Req(PUT, s"$kgBase/views/${orgId}/bands/composite", headersJsonUser, view.toEntity)).mapResp { resp =>
        resp.status shouldEqual StatusCodes.Created
      }
    }

    waitForView()
  }

  "searching the projections" should {
    "find all bands" in {
      eventually {
        cl(
          Req(
            POST,
            s"$kgBase/views/${orgId}/bands/composite/projections/bands/_search",
            headersJsonUser,
            sortAscendingById.toEntity
          )
        ).mapJson { (json, resp) =>
          json.getJson("hits").getArray("hits").map(_.getJson("_source")) shouldEqual jsonContentOf(
            "/kg/views/composite/bands-results1.json"
          ).asArray.value
          resp.status shouldEqual StatusCodes.OK
        }
      }
    }
    "find all albums" in {
      eventually {
        cl(
          Req(
            POST,
            s"$kgBase/views/${orgId}/bands/composite/projections/albums/_search",
            headersJsonUser,
            sortAscendingById.toEntity
          )
        ).mapJson { (json, resp) =>
          json.getJson("hits").getArray("hits").map(_.getJson("_source")) shouldEqual jsonContentOf(
            "/kg/views/composite/albums-results1.json"
          ).asArray.value
          resp.status shouldEqual StatusCodes.OK
        }
      }
    }
  }

  "uploading more data" should {
    "upload more songs" in {
      jsonContentOf("/kg/views/composite/songs2.json").asArray.value.foreach { song =>
        cl(Req(POST, s"$kgBase/resources/$songsProject", headersJsonUser, song.toEntity)).mapResp { resp =>
          resp.status shouldEqual StatusCodes.Created
        }
      }
    }
  }

  "waiting for data to be indexed" should {
    waitForView()
  }

  "searching the projections with more data" should {
    "find all bands" in {
      eventually {
        cl(
          Req(
            POST,
            s"$kgBase/views/${orgId}/bands/composite/projections/bands/_search",
            headersJsonUser,
            sortAscendingById.toEntity
          )
        ).mapJson { (json, resp) =>
          json.getJson("hits").getArray("hits").map(_.getJson("_source")) shouldEqual jsonContentOf(
            "/kg/views/composite/bands-results2.json"
          ).asArray.value
          resp.status shouldEqual StatusCodes.OK
        }
      }
    }
    "find all albums" in {
      eventually {
        cl(
          Req(
            POST,
            s"$kgBase/views/${orgId}/bands/composite/projections/albums/_search",
            headersJsonUser,
            sortAscendingById.toEntity
          )
        ).mapJson { (json, resp) =>
          json.getJson("hits").getArray("hits").map(_.getJson("_source")) shouldEqual jsonContentOf(
            "/kg/views/composite/albums-results2.json"
          ).asArray.value
          resp.status shouldEqual StatusCodes.OK
        }
      }
    }
  }

  val sortAscendingById = Json
    .obj(
      "sort" -> Json.arr(
        Json.obj(
          "@id" -> Json.obj(
            "order" -> Json.fromString("asc")
          )
        )
      )
    )

  def waitForView() = "wait for view" should {
    "wait for view to be indexed" in {
      eventually(
        cl(Req(GET, s"$kgBase/views/${orgId}/bands/composite/projections/_/statistics", headersJsonUser)).mapJson {
          (json, resp) =>
            json.getArray("_results").foreach { stats =>
              stats.getLong("totalEvents") should be > 0L
              stats.getLong("remainingEvents") shouldEqual 0
            }
            resp.status shouldEqual StatusCodes.OK
        }
      )
    }

    "reset the view" in {
      cl(Req(DELETE, s"$kgBase/views/${orgId}/bands/composite/projections/_/offset", headersJsonUser)).mapResp {
        resp =>
        println(resp)
          resp.status shouldEqual StatusCodes.OK
      }
    }

    "wait for view to be indexed again" in {
      eventually(
        cl(Req(GET, s"$kgBase/views/${orgId}/bands/composite/projections/_/statistics", headersJsonUser)).mapJson {
          (json, resp) =>
            json.getArray("_results").foreach { stats =>
              stats.getLong("totalEvents") should be > 0L
              stats.getLong("remainingEvents") shouldEqual 0
            }
            resp.status shouldEqual StatusCodes.OK
        }
      )
    }
  }

}
