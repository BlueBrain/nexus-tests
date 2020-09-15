package ch.epfl.bluebrain.nexus.tests.kg

import java.net.URLEncoder
import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import cats.implicits._
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.CirceEq
import ch.epfl.bluebrain.nexus.tests.HttpClientDsl._
import ch.epfl.bluebrain.nexus.tests.Identity.UserCredentials
import ch.epfl.bluebrain.nexus.tests.Optics._
import ch.epfl.bluebrain.nexus.tests.Tags.ResourcesTag
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

    "add necessary permissions for user" taggedAs ResourcesTag in {
      aclDsl.addPermission(
        "/",
        Rick,
        Organizations.Create
      ).runSyncUnsafe()
    }

    "succeed if payload is correct" taggedAs ResourcesTag in {
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
    "create a schema" taggedAs ResourcesTag in {
      val schemaPayload = jsonContentOf("/kg/schemas/simple-schema.json")

      cl.put[Json](s"/schemas/$id1/test-schema", schemaPayload, Rick) {
        (_, response) =>
          response.status shouldEqual StatusCodes.Created
      }.runSyncUnsafe()
    }

    "creating a schema with property shape" taggedAs ResourcesTag in {
      val schemaPayload = jsonContentOf("/kg/schemas/simple-schema-prop-shape.json")

      cl.post[Json](s"/schemas/$id1", schemaPayload, Rick) {
        (_, response) =>
          response.status shouldEqual StatusCodes.Created
      }.runSyncUnsafe()
    }

    "creating a schema that imports the property shape schema" taggedAs ResourcesTag in {
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
    "succeed if the payload is correct" taggedAs ResourcesTag in {
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

    "fetch the payload wih metadata" taggedAs ResourcesTag in {
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

    "fetch the original payload" taggedAs ResourcesTag in {
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
          quote("{realm}")  -> Rick.realm.name
        )
      )

    "fail if the schema doesn't exist in the project" taggedAs ResourcesTag in {
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

    "fail to create a cross-project-resolver for proj2 if identities are missing" taggedAs ResourcesTag in {
      cl.post[Json](s"/resolvers/$id2", filterKey("identities")(resolverPayload), Rick) {
        (_, response) =>
          response.status shouldEqual StatusCodes.BadRequest
      }.runSyncUnsafe()
    }

    "create a cross-project-resolver for proj2" taggedAs ResourcesTag in {
      cl.post[Json](s"/resolvers/$id2", resolverPayload, Rick) {
        (_, response) =>
          response.status shouldEqual StatusCodes.Created
      }.runSyncUnsafe()
    }

    "update a cross-project-resolver for proj2" taggedAs ResourcesTag in {
      val updated = resolverPayload deepMerge Json.obj("priority" -> Json.fromInt(20))
      eventually {
        cl.put[Json](s"/resolvers/$id2/example-id?rev=1", updated, Rick) { (_, response) =>
          response.status shouldEqual StatusCodes.OK
        }.runSyncUnsafe()
      }
    }

    "fetch the update" taggedAs ResourcesTag in {
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

    "wait for the cross-project resolver to be indexed" taggedAs ResourcesTag in {
      val expected = jsonContentOf(
        "/kg/resources/cross-project-resolver-list.json",
        Map(
          quote("{deltaBase}")  -> config.deltaUri.toString(),
          quote("{projId}")  -> s"$id2",
          quote("{project}") -> s"${config.deltaUri}/projects/$id2",
          quote("{realm}")   -> Rick.realm.name,
          quote("{user}")    -> Rick.name
        )
      )

      eventually {
        cl.get[Json](s"/resolvers/$id2", Rick) { (json, response) =>
          response.status shouldEqual StatusCodes.OK
          filterSearchMetadata(json) shouldEqual expected
        }.runSyncUnsafe()
      }
    }

    s"fetches a resource in project '$id1' through project '$id2' resolvers" taggedAs ResourcesTag in {
      cl.get[Json](s"/schemas/$id1/test-schema", Rick) { (json, response1) =>
        response1.status shouldEqual StatusCodes.OK
        runTask {
          for {
            _ <- cl.get[Json](s"/resolvers/$id2/_/test-schema", Rick) { (jsonResolved, response2) =>
              response2.status shouldEqual StatusCodes.OK
              jsonResolved should equalIgnoreArrayOrder(json)
            }
            _ <- cl.get[Json](s"/resolvers/$id2/example-id/test-schema", Rick) { (jsonResolved, response2) =>
              response2.status shouldEqual StatusCodes.OK
              jsonResolved should equalIgnoreArrayOrder(json)
            }
          } yield {
            succeed
          }
        }
      }.runSyncUnsafe()

      cl.get[Json](s"/resolvers/$id2/example-id/test-schema-2", Rick) { (_, response) =>
        response.status shouldEqual StatusCodes.NotFound
      }.runSyncUnsafe()

      cl.get[Json](s"/resolvers/$id2/_/test-schema-2", Rick) { (_, response) =>
        response.status shouldEqual StatusCodes.NotFound
      }.runSyncUnsafe()
    }

    "resolve schema from the other project" taggedAs ResourcesTag in {
      val payload = jsonContentOf(
        "/kg/resources/simple-resource.json",
        Map(quote("{priority}") -> "3", quote("{resourceId}") -> "1")
      )

      eventually {
        cl.put[Json](s"/resources/$id2/test-schema/test-resource:1", payload, Rick) { (_, response) =>
          response.status shouldEqual StatusCodes.Created
        }.runSyncUnsafe()
      }
    }
  }

  "updating a resource" should {
    "send the update" taggedAs ResourcesTag in {
      val payload = jsonContentOf(
        "/kg/resources/simple-resource.json",
        Map(quote("{priority}") -> "3", quote("{resourceId}") -> "1")
      )

      cl.put[Json](s"/resources/$id1/test-schema/test-resource:1?rev=1", payload, Rick) { (_, response) =>
        response.status shouldEqual StatusCodes.OK
      }.runSyncUnsafe()
    }

    "fetch the update" taggedAs ResourcesTag in {
      val expected = jsonContentOf(
        "/kg/resources/simple-resource-response.json",
        Map(
          quote("{priority}")  -> "3",
          quote("{rev}")       -> "2",
          quote("{resources}") -> s"${config.deltaUri}/resources/$id1",
          quote("{project}")   -> s"${config.deltaUri}/projects/$id1",
          quote("{deltaBase}")   -> config.deltaUri.toString(),
          quote("{realm}")     -> Rick.realm.name,
          quote("{user}")      -> Rick.name
        )
      )
      List(
        s"/resources/$id1/test-schema/test-resource:1",
        s"/resources/$id1/_/test-resource:1"
      ).traverse { url =>
        cl.get[Json](url, Rick) { (json, response) =>
          response.status shouldEqual StatusCodes.OK
          filterMetadataKeys(json) should equalIgnoreArrayOrder(expected)
        }
      }.runSyncUnsafe()
    }

    "fetch previous revision" taggedAs ResourcesTag in {
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

      List(
        s"/resources/$id1/test-schema/test-resource:1?rev=1",
        s"/resources/$id1/_/test-resource:1?rev=1"
      ).traverse { url =>
        cl.get[Json](url, Rick) { (json, response) =>
          response.status shouldEqual StatusCodes.OK
          filterMetadataKeys(json) should equalIgnoreArrayOrder(expected)
        }
      }.runSyncUnsafe()
    }
  }

  "tagging a resource" should {
    "create a tag" taggedAs ResourcesTag in {
      val tag1 = jsonContentOf("/kg/resources/tag.json", Map(quote("{tag}") -> "v1.0.0", quote("{rev}") -> "1"))
      val tag2 = jsonContentOf("/kg/resources/tag.json", Map(quote("{tag}") -> "v1.0.1", quote("{rev}") -> "2"))

      {
        for {
          _ <- cl.post[Json](s"/resources/$id1/test-schema/test-resource:1/tags?rev=2", tag1, Rick) {
            (_, response) => response.status shouldEqual StatusCodes.Created
          }
          _ <- cl.post[Json](s"/resources/$id1/_/test-resource:1/tags?rev=3", tag2, Rick) {
            (_, response) => response.status shouldEqual StatusCodes.Created
          }
        } yield succeed
      }.runSyncUnsafe()
    }


    "fetch a tagged value" taggedAs ResourcesTag in {
      val expectedTag1 = jsonContentOf(
        "/kg/resources/simple-resource-response.json",
        Map(
          quote("{priority}")  -> "3",
          quote("{rev}")       -> "2",
          quote("{resources}") -> s"${config.deltaUri}/resources/$id1",
          quote("{project}")   -> s"${config.deltaUri}/projects/$id1",
          quote("{deltaBase}")   -> config.deltaUri.toString(),
          quote("{realm}")     -> Rick.realm.name,
          quote("{user}")      -> Rick.name
        )
      )

      cl.get[Json](s"/resources/$id1/test-schema/test-resource:1?tag=v1.0.1", Rick) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK
          filterMetadataKeys(json) should equalIgnoreArrayOrder(expectedTag1)
      }.runSyncUnsafe()

      val expectedTag2 = jsonContentOf(
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

      cl.get[Json](s"/resources/$id1/_/test-resource:1?tag=v1.0.0", Rick) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK
          filterMetadataKeys(json) should equalIgnoreArrayOrder(expectedTag2)
      }.runSyncUnsafe()
    }
  }

  "listing resources" should {

    "list default resources" taggedAs ResourcesTag in {
      val mapping = Map(
        quote("{project-label}") -> id1,
        quote("{project}")       -> s"${config.deltaUri}/projects/$id1",
        quote("{deltaBase}")     -> config.deltaUri.toString(),
        quote("{realm}")         -> Rick.realm.name,
        quote("{user}")          -> Rick.name
      )
      val resources = List(
        "resolvers" -> jsonContentOf("/kg/listings/default-resolver.json", mapping),
        "views"     -> jsonContentOf("/kg/listings/default-view.json", mapping),
        "storages"  -> jsonContentOf("/kg/listings/default-storage.json", mapping)
      )

      resources.traverse { case (segment, expected) =>
          cl.get[Json](s"/$segment/$id1", Rick) { (json, response) =>
            response.status shouldEqual StatusCodes.OK
            filterSearchMetadata(json) shouldEqual expected
          }
      }.runSyncUnsafe()
    }

    "add more resource to the project" taggedAs ResourcesTag in {
      (2 to 5).toList.traverse { resourceId =>
        val payload = jsonContentOf(
          "/kg/resources/simple-resource.json",
          Map(quote("{priority}") -> "3", quote("{resourceId}") -> s"$resourceId")
        )
        cl.put[Json](s"/resources/$id1/test-schema/test-resource:$resourceId", payload, Rick) { (_, response) =>
          response.status shouldEqual StatusCodes.Created
        }
      }.runSyncUnsafe()
    }

    "list the resources" taggedAs ResourcesTag in {
      val expected = jsonContentOf(
        "/kg/listings/response.json",
        Map(
          quote("{resources}") -> s"${config.deltaUri}/resources/$id1",
          quote("{project}")   -> s"${config.deltaUri}/projects/$id1",
          quote("{deltaBase}")   -> config.deltaUri.toString(),
          quote("{realm}")     -> Rick.realm.name,
          quote("{user}")      -> Rick.name
        )
      )

      eventually {
        cl.get[Json](s"/resources/$id1/test-schema", Rick) { (json, response) =>
          response.status shouldEqual StatusCodes.OK
          filterSearchMetadata(json) shouldEqual expected
        }.runSyncUnsafe()
      }
    }

    "return 400 when using both from and after" taggedAs ResourcesTag in {
      cl.get[Json](s"/resources/$id1/test-schema?from=10&after=%5B%22test%22%5D", Rick) {
        (json, response) =>
          response.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/kg/listings/from-and-after-error.json")
      }.runSyncUnsafe()
    }

    "return 400 when from is bigger than limit" taggedAs ResourcesTag in {
      cl.get[Json](s"/resources/$id1/test-schema?from=10001", Rick) {
        (json, response) =>
          response.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/kg/listings/from-over-limit-error.json")
      }.runSyncUnsafe()
    }
  }

}
