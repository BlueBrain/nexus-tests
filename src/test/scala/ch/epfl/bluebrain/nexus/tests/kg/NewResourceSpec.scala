package ch.epfl.bluebrain.nexus.tests.kg

import java.net.URLEncoder
import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.CirceEq
import ch.epfl.bluebrain.nexus.tests.HttpClientDsl._
import ch.epfl.bluebrain.nexus.tests.Identity.UserCredentials
import ch.epfl.bluebrain.nexus.tests.Optics._
import ch.epfl.bluebrain.nexus.tests.Tags.{KgTag, ResourcesTag}
import ch.epfl.bluebrain.nexus.tests.iam.types.Permission.Organizations
import ch.epfl.bluebrain.nexus.tests.{Identity, NewBaseSpec, Realm}
import io.circe.Json
import monix.execution.Scheduler.Implicits.global
import org.scalatest.concurrent.Eventually

class NewResourceSpec extends NewBaseSpec with Eventually with CirceEq {

  private val testRealm   = Realm("resources" + genString())
  private val testClient = Identity.ClientCredentials(genString(), genString(), testRealm)
  private val Rick = UserCredentials(genString(), genString(), testRealm)
  private val Morty = UserCredentials(genString(), genString(), testRealm)

  private val orgId   = genId()
  private val projId1 = genId()
  private val projId2 = genId()
  private val id1     = s"$orgId/$projId1"
  private val id2     = s"$orgId/$projId2"
  println(id1 + id2)

  override def beforeAll(): Unit = {
    super.beforeAll()
    initRealm(
      testRealm,
      Identity.ServiceAccount,
      testClient,
      Rick :: Morty :: Nil
    ).runSyncUnsafe()
  }

  "creating projects" should {

    "add necessary permissions for user" taggedAs(KgTag, ResourcesTag) in {
      aclDsl.addPermission(
        "/",
        Rick,
        Organizations.Create
      ).runSyncUnsafe()
    }

    "succeed if payload is correct" taggedAs(KgTag, ResourcesTag) in {
      {
        for {
          _ <- adminDsl.createOrganization(orgId, orgId, Rick)
          _ <- adminDsl.createProject(orgId, projId1, kgDsl.projectJson(name = id1), Rick)
          _ <- adminDsl.createProject(orgId, projId2, kgDsl.projectJson(name = id2), Rick)
        } yield {}
      }.runSyncUnsafe()
    }
  }

  "adding schema" should {
    "create a schema" taggedAs(KgTag, ResourcesTag) in {
      val schemaPayload = jsonContentOf("/kg/schemas/simple-schema.json")

      cl.put[Json](s"/schemas/$id1/test-schema", schemaPayload, Rick) {
        (_, response) =>
          response.status shouldEqual StatusCodes.Created
      }.runSyncUnsafe()
    }

    "creating a schema with property shape" taggedAs(KgTag, ResourcesTag) in {
      val schemaPayload = jsonContentOf("/kg/schemas/simple-schema-prop-shape.json")

      cl.post[Json](s"/schemas/$id1", schemaPayload, Rick) {
        (_, response) =>
          response.status shouldEqual StatusCodes.Created
      }.runSyncUnsafe()
    }

    "creating a schema that imports the property shape schema" taggedAs(KgTag, ResourcesTag) in {
      val schemaPayload = jsonContentOf("/kg/schemas/simple-schema-imports.json")

      eventually {
        cl.post[Json](s"/schemas/$id1", schemaPayload, Rick) {
          (_, response) =>
            response.status shouldEqual StatusCodes.Created
        }.runSyncUnsafe()
      }
    }
  }

  "creating a resource" should {
    "succeed if the payload is correct" taggedAs(KgTag, ResourcesTag) in {
      val payload =
        jsonContentOf(
          "/kg/resources/simple-resource.json",
          Map(
            quote("{priority}") -> "5",
            quote("{resourceId}") -> "1"
          )
        )

      cl.put[Json](s"/resources/$id1/test-schema/test-resource:1", payload, Rick) {
        (_, response) =>
          response.status shouldEqual StatusCodes.Created
      }.runSyncUnsafe()

      val id2 = URLEncoder.encode("https://dev.nexus.test.com/test-schema-imports", "UTF-8")
      val payload2 = jsonContentOf(
          "/kg/resources/simple-resource.json",
          Map(
            quote("{priority}") -> "5",
            quote("{resourceId}") -> "10"
          )
        )

      cl.put[Json](s"/resources/$id1/$id2/test-resource:10", payload2, Rick) {
        (_, response) =>
          response.status shouldEqual StatusCodes.Created
      }.runSyncUnsafe()
    }

    "fetch the payload wih metadata" taggedAs(KgTag, ResourcesTag) in {
      cl.get[Json](s"/resources/$id1/test-schema/test-resource:1", Rick) { (json, response) =>
        val expected = jsonContentOf(
          "/kg/resources/simple-resource-response.json",
          Map(
            quote("{priority}")  -> "5",
            quote("{rev}")       -> "1",
            quote("{resources}") -> s"${config.deltaUri}/resources/$id1",
            quote("{project}")   -> s"${config.deltaUri}/projects/$id1",
            quote("{deltaBase}")   -> config.deltaUri.toString(),
            quote("{realm}")     -> Rick.realm.name,
            quote("{user}")      -> Rick.name
          )
        )
        response.status shouldEqual StatusCodes.OK
        filterMetadataKeys(json) should equalIgnoreArrayOrder(expected)
      }.runSyncUnsafe()
    }

    "fetch the original payload" taggedAs(KgTag, ResourcesTag) in {
      cl.get[Json](s"/resources/$id1/test-schema/test-resource:1/source", Rick) { (json, response) =>
        val expected =
          jsonContentOf(
            "/kg/resources/simple-resource.json",
            Map(quote("{priority}") -> "5", quote("{resourceId}") -> "1")
          )
        response.status shouldEqual StatusCodes.OK
        json should equalIgnoreArrayOrder(expected)
      }.runSyncUnsafe()
    }
  }

  "cross-project resolvers" should {
    val resolverPayload =
      jsonContentOf(
        "/kg/resources/cross-project-resolver.json",
        Map(
          quote("{project}") -> id1,
          quote("{user}")  -> Rick.name,
          quote("{user}")  -> Rick.realm.name
        )
      )

    "fail if the schema doesn't exist in the project" taggedAs(KgTag, ResourcesTag) in {
      val payload = jsonContentOf(
        "/kg/resources/simple-resource.json",
        Map(
          quote("{priority}") -> "3",
          quote("{resourceId}") -> "1"
        )
      )

      cl.put[Json](s"/resources/$id2/test-schema/test-resource:1", payload, Rick) {
        (_, response) =>
          response.status shouldEqual StatusCodes.NotFound
      }.runSyncUnsafe()
    }

    "fail to create a cross-project-resolver for proj2 if identities are missing" taggedAs(KgTag, ResourcesTag) in {
      cl.post[Json](s"/resolvers/$id2", filterKey("identities")(resolverPayload), Rick) {
        (_, response) =>
          response.status shouldEqual StatusCodes.BadRequest
      }.runSyncUnsafe()
    }

    "create a cross-project-resolver for proj2" taggedAs(KgTag, ResourcesTag) in {
      cl.post[Json](s"/resolvers/$id2", resolverPayload, Rick) {
        (_, response) =>
          response.status shouldEqual StatusCodes.Created
      }.runSyncUnsafe()
    }

    "update a cross-project-resolver for proj2" taggedAs(KgTag, ResourcesTag) in {
      val updated = resolverPayload deepMerge Json.obj("priority" -> Json.fromInt(20))
      eventually {
        cl.put[Json](s"/resolvers/$id2/example-id?rev=1", updated, Rick) { (_, response) =>
          response.status shouldEqual StatusCodes.OK
        }.runSyncUnsafe()
      }
    }

    "fetch the update" taggedAs(KgTag, ResourcesTag) in {
      val expected = jsonContentOf(
          "/kg/resources/cross-project-resolver-updated-resp.json",
          Map(
            quote("{project}")        -> id1,
            quote("{resources}")      -> s"${config.deltaUri}/resolvers/$id2",
            quote("{project-parent}") -> s"${config.deltaUri}/projects/$id2",
            quote("{iamBase}")        -> config.deltaUri.toString(),
            quote("{realm}")          -> Rick.realm.name,
            quote("{user}")           -> Rick.name
          )
        )

      cl.get[Json](s"/resolvers/$id2/example-id", Rick) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        filterMetadataKeys(json) should equalIgnoreArrayOrder(expected)
      }.runSyncUnsafe()
    }
  }

}
