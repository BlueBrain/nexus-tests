package ch.epfl.bluebrain.nexus.tests.kg

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import cats.implicits._
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.HttpClientDsl._
import ch.epfl.bluebrain.nexus.tests.Identity.UserCredentials
import ch.epfl.bluebrain.nexus.tests.Optics._
import ch.epfl.bluebrain.nexus.tests.Tags.CompositeViewsTag
import ch.epfl.bluebrain.nexus.tests.iam.types.Permission.{Events, Organizations, Views}
import ch.epfl.bluebrain.nexus.tests.{Identity, NewBaseSpec, Realm}
import com.typesafe.scalalogging.Logger
import io.circe.Json
import io.circe.optics.JsonPath._
import monix.execution.Scheduler.Implicits.global

class NewCompositeViewsSpec extends NewBaseSpec {

  private val logger = Logger[this.type]

  case class Stats(totalEvents: Long, remainingEvents: Long)

  object Stats {
    import io.circe._
    import io.circe.generic.semiauto._
    implicit val decoder: Decoder[Stats] = deriveDecoder[Stats]
    implicit val encoder: Encoder.AsObject[Stats] = deriveEncoder[Stats]
  }

  private val orgId        = genId()
  private val bandsProject = "bands"
  private val albumsProject = "albums"
  private val songsProject = "songs"

  private val testRealm   = Realm("composite" + genString())
  private val testClient = Identity.ClientCredentials(genString(), genString(), testRealm)
  private val Tom = UserCredentials(genString(), genString(), testRealm)
  private val Jerry = UserCredentials(genString(), genString(), testRealm)

  override def beforeAll(): Unit = {
    super.beforeAll()
    initRealm(
      testRealm,
      Identity.ServiceAccount,
      testClient,
      Tom :: Jerry :: Nil
    ).runSyncUnsafe()
  }

  "Creating projects" should {
    "add necessary permissions for user" taggedAs CompositeViewsTag in {
      aclDsl.addPermissions(
        s"/$orgId",
        Jerry,
        Set(Organizations.Create, Views.Query, Events.Read)
      )
    }

    "succeed if payload is correct" taggedAs CompositeViewsTag in {
      val projectPayload = jsonContentOf("/kg/views/composite/project.json")
      for {
        _ <- adminDsl.createOrganization(orgId, orgId, Jerry)
        _ <- adminDsl.createProject(orgId, bandsProject, projectPayload, Jerry)
        _ <- adminDsl.createProject(orgId, albumsProject, projectPayload, Jerry)
        _ <- adminDsl.createProject(orgId, songsProject, projectPayload, Jerry)
      } yield succeed
    }

    "wait until in project resolver is created" taggedAs CompositeViewsTag in {
      eventually {
          cl.get[Json](s"/resolvers/$orgId/$bandsProject", Jerry) { (json, response) =>
            response.status shouldEqual StatusCodes.OK
            _total.getOption(json).value shouldEqual 1
          }
      }
      eventually {
        cl.get[Json](s"/resolvers/$orgId/$albumsProject", Jerry) { (json, response) =>
          response.status shouldEqual StatusCodes.OK
          _total.getOption(json).value shouldEqual 1
        }
      }
      eventually {
        cl.get[Json](s"/resolvers/$orgId/$songsProject", Jerry) { (json, response) =>
          response.status shouldEqual StatusCodes.OK
          _total.getOption(json).value shouldEqual 1
        }
      }
    }
  }

  "Uploading data" should {
    "upload context" taggedAs CompositeViewsTag in {
      val context = jsonContentOf("/kg/views/composite/context.json")
      List(songsProject, albumsProject, bandsProject).traverse { projectId =>
        cl.post[Json](s"/resources/$orgId/$projectId", context, Jerry) {
          (_, response) => response.status shouldEqual StatusCodes.Created
        }
      }
    }

    "upload songs" taggedAs CompositeViewsTag in {
      root.each.json.getAll(
        jsonContentOf("/kg/views/composite/songs1.json")
      ).traverse { song =>
        cl.post[Json](s"/resources/$orgId/$songsProject", song, Jerry) {
          (_, response) => response.status shouldEqual StatusCodes.Created
        }
      }
    }

    "upload albums" taggedAs CompositeViewsTag in {
      root.each.json.getAll(
        jsonContentOf("/kg/views/composite/albums.json")
      ).traverse { song =>
        cl.post[Json](s"/resources/$orgId/$albumsProject", song, Jerry) {
          (_, response) => response.status shouldEqual StatusCodes.Created
        }
      }
    }

    "upload bands" taggedAs CompositeViewsTag in {
      root.each.json.getAll(
        jsonContentOf("/kg/views/composite/bands.json")
      ).traverse { song =>
        cl.post[Json](s"/resources/$orgId/$bandsProject", song, Jerry) {
          (_, response) => response.status shouldEqual StatusCodes.Created
        }
      }
    }
  }

  "creating the view" should {

    def jerryToken = tokensMap.get(Jerry).credentials.token()

    "create a composite view" taggedAs CompositeViewsTag in {
      val view = jsonContentOf(
        "/kg/views/composite/composite-view.json",
        replacements(
          Jerry,
          quote("{org}")               -> orgId,
          quote("{org2}")              -> orgId,
          quote("{remoteEndpoint}")    -> "http://delta:8080/v1",
          quote("{token}")             -> jerryToken
        )
      )

      cl.put[Json](s"/views/${orgId}/bands/composite", view, Jerry) {
        (_, response) =>
          response.status shouldEqual StatusCodes.Created
      }
    }

    "wait for data to be indexed after creation" taggedAs CompositeViewsTag in
      waitForView()

    "reject creating a composite view with remote source endpoint with a wrong suffix" taggedAs CompositeViewsTag in {
      val view = jsonContentOf(
        "/kg/views/composite/composite-view.json",
        replacements(
          Jerry,
          quote("{org}")               -> orgId,
          quote("{org2}")              -> orgId,
          quote("{remoteEndpoint}")    -> "http://delta:8080/v1/other",
          quote("{token}")             -> jerryToken
        )
      )

      cl.put[Json](s"/views/${orgId}/bands/composite2", view, Jerry) {
        (_, response) =>
          response.status shouldEqual StatusCodes.NotFound
      }
    }

    "reject creating a composite view with wrong remote source token" taggedAs CompositeViewsTag in {
      val view = jsonContentOf(
        "/kg/views/composite/composite-view.json",
        replacements(
          Jerry,
          quote("{org}")               -> orgId,
          quote("{org2}")              -> orgId,
          quote("{remoteEndpoint}")    -> "http://delta:8080/v1",
          quote("{token}")             -> s"${jerryToken}wrong"
        )
      )

      cl.put[Json](s"/views/${orgId}/bands/composite2", view, Jerry) {
        (json, response) =>
          response.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/kg/views/composite/composite-source-token-reject.json")
      }
    }

    "reject creating a composite view with remote source endpoint with a wrong hostname" taggedAs CompositeViewsTag in {
      val view = jsonContentOf(
        "/kg/views/composite/composite-view.json",
        replacements(
          Jerry,
          quote("{org}")               -> orgId,
          quote("{org2}")              -> orgId,
          quote("{remoteEndpoint}")    -> "http://fail/v1",
          quote("{token}")             -> jerryToken
        )
      )

      cl.put[Json](s"/views/${orgId}/bands/composite2", view, Jerry) {
        (_, response) =>
          response.status shouldEqual StatusCodes.BadRequest
      }
    }
  }

  private val sortAscendingById = Json
    .obj(
      "sort" -> Json.arr(
        Json.obj(
          "@id" -> Json.obj(
            "order" -> Json.fromString("asc")
          )
        )
      )
    )

  "searching the projections" should {
    "find all bands" taggedAs CompositeViewsTag in {
      eventually {
        cl.post[Json](s"/views/${orgId}/bands/composite/projections/bands/_search", sortAscendingById, Jerry) {
          (json, response) =>
            response.status shouldEqual StatusCodes.OK
            hitsSource.getAll(json) should contain theSameElementsInOrderAs root.arr.getOption(
              jsonContentOf(
              "/kg/views/composite/bands-results1.json"
              )
            ).value
        }
      }
    }

    "find all albums" taggedAs CompositeViewsTag in {
      eventually {
        cl.post[Json](s"/views/${orgId}/bands/composite/projections/albums/_search", sortAscendingById, Jerry) {
          (json, response) =>
            response.status shouldEqual StatusCodes.OK
            hitsSource.getAll(json) should contain theSameElementsInOrderAs root.arr.getOption(
              jsonContentOf(
                "/kg/views/composite/albums-results1.json"
              )
            ).value
        }
      }
    }
  }

  private def waitForView() = {
    eventually {
      cl.get[Json](s"/views/${orgId}/bands/composite/projections/_/statistics", Jerry) {
        (json, response) =>
          root._results.each.as[Stats].getAll(json).foreach { stat =>
            logger.debug(s"totalEvents: ${stat.totalEvents}, remainingEvents: ${stat.remainingEvents}")
            stat.totalEvents should be > 0L
            stat.remainingEvents shouldEqual 0
          }
          response.status shouldEqual StatusCodes.OK
      }
    }
  }

  "Delete composite views" should {

//    "be ok" taggedAs CompositeViewsTag in {
//      cl.delete[Json](s"/views/${orgId}/bands/composite?rev=1", Jerry){
//        (_, response) => response.status shouldEqual StatusCodes.OK
//      }
//    }

  }


}
