package ch.epfl.bluebrain.nexus.tests.admin

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import cats.implicits._
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.EitherValues
import ch.epfl.bluebrain.nexus.tests.DeltaHttpClient._
import ch.epfl.bluebrain.nexus.tests.Identity.{Authenticated, UserCredentials}
import ch.epfl.bluebrain.nexus.tests.Optics._
import ch.epfl.bluebrain.nexus.tests.Tags.OrgsTag
import ch.epfl.bluebrain.nexus.tests.config.ConfigLoader._
import ch.epfl.bluebrain.nexus.tests.config.PrefixesConfig
import ch.epfl.bluebrain.nexus.tests.iam.types.{AclListing, Permission}
import ch.epfl.bluebrain.nexus.tests.{Identity, NewBaseSpec}
import com.typesafe.config.ConfigFactory
import io.circe.Json
import monix.execution.Scheduler.Implicits.global
import org.scalatest.OptionValues

class OrgsSpec extends NewBaseSpec with OptionValues with EitherValues{

  private val testRealm   = "orgs" + genString()
  private val testClient = Identity.ClientCredentials(genString(), genString())
  private val Fry = UserCredentials(genString(), genString())
  private val Leela = UserCredentials(genString(), genString())

  val prefixesConfig: PrefixesConfig = load[PrefixesConfig](ConfigFactory.load(), "prefixes")

  private[tests] val errorCtx = Map(quote("{error-context}") -> prefixesConfig.errorContext.toString)

  import ch.epfl.bluebrain.nexus.tests.iam.types.Permission._

  override def beforeAll(): Unit = {
    super.beforeAll()
    initRealm(
      testRealm,
      Identity.ServiceAccount,
      testClient,
      Fry :: Leela :: Nil
    ).runSyncUnsafe()
  }

  "creating an organization" should {
    "fail if the permissions are missing" taggedAs OrgsTag in {
      cl.put[Json](s"/orgs/${genId()}", Json.obj(), Fry){
        (json, response) =>
          response.status shouldEqual StatusCodes.Forbidden
          json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json")
      }.runSyncUnsafe()
    }

    "add necessary permissions for user" taggedAs OrgsTag in {
      addPermission(
        "/",
        Fry,
        testRealm,
        Organizations.Create
      ).runSyncUnsafe()
    }

    val id = genId()
    "succeed if payload is correct"  taggedAs OrgsTag in {
      cl.put[Json](s"/orgs/$id", orgPayload(id), Fry) {
        (json, response) =>
          response.status shouldEqual StatusCodes.Created
          filterMetadataKeys(json) shouldEqual createRespJson(
            id,
            1L, "orgs",
            "Organization",
            Fry
          )
      }.runSyncUnsafe()
    }

    "check if permissions have been created for user" taggedAs OrgsTag in {
      cl.get[AclListing](s"/acls/$id", Fry) { (acls, response) =>
        response.status shouldEqual StatusCodes.OK
        acls._results.head.acl.head.permissions shouldEqual Permission.organizationPermissions
      }
    }

    "fail if organization already exists"  taggedAs OrgsTag in {
      val duplicate = genId()

      val expected = List(
        StatusCodes.Created -> createRespJson(
          duplicate,
          1L,
          "orgs",
          "Organization",
          Fry
        ),
        StatusCodes.Conflict -> jsonContentOf(
          "/admin/errors/org-already-exists.json",
          Map(quote("{orgId}") -> duplicate)
        )
      )

      expected.traverse { case (status, expectedJson) =>
        cl.put[Json](s"/orgs/$duplicate", orgPayload(duplicate), Fry) {
          (json, response) =>
            response.status shouldEqual status
            filterMetadataKeys(json) shouldEqual expectedJson
        }
      }.runSyncUnsafe()
    }
  }

  "fetching an organization" should {
    val id = genId()
    "fail if the permissions are missing"  taggedAs OrgsTag in {
      cl.put[Json](s"/orgs/$id", orgPayload(s"Description $id"), Fry) {
        (json, response) =>
          response.status shouldEqual StatusCodes.Created
          filterMetadataKeys(json) shouldEqual createRespJson(
            id,
            1L,
            "orgs",
            "Organization",
            Fry
          )
      }.runSyncUnsafe()

      cl.get[Json](s"/orgs/$id", Leela) { (json, response) =>
        response.status shouldEqual StatusCodes.Forbidden
        json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json", errorCtx)
      }.runSyncUnsafe()
    }

    "add orgs/read permissions for user"  taggedAs OrgsTag in {
      addPermission(
        "/",
        Leela,
        testRealm,
        Organizations.Read
      ).runSyncUnsafe()
    }

    "succeed if organization exists"  taggedAs OrgsTag in {
      cl.get[Json](s"/orgs/$id", Leela) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        admin.validate(json, "Organization", "orgs", id, s"Description $id", 1L, id)
      }.runSyncUnsafe()
    }

    "fetch organization by UUID"  taggedAs OrgsTag in {
      cl.get[Json](s"/orgs/$id", Leela) { (jsonById, _) =>
        runTask {
          val orgUuid = admin._uuid.getOption(jsonById).value

          cl.get[Json](s"/orgs/$orgUuid", Leela) { (jsonByUuid, response) =>
            response.status shouldEqual StatusCodes.OK
            jsonByUuid shouldEqual jsonById
          }
        }
      }
    }

    "return not found when fetching a non existing revision of an organizations"  taggedAs OrgsTag in {
      cl.get[Json](s"/orgs/$id?rev=3", Leela) { (_, response) =>
        response.status shouldEqual StatusCodes.NotFound
      }
    }

    val nonExistent = genId()
    "add orgs/read permissions for non-existing organization"  taggedAs OrgsTag in {
      addPermission(
        s"/$nonExistent",
        Leela,
        testRealm,
        Organizations.Create
      ).runSyncUnsafe()
    }

    "return not found when fetching a non existing organization"  taggedAs OrgsTag in {
      cl.get[Json](s"/orgs/$nonExistent", Leela) { (_, response) =>
        response.status shouldEqual StatusCodes.NotFound
      }
    }
  }

  "updating an organization" should {
    val id = genString()
    val create = orgPayload(s"$id organization")

    "fail if the permissions are missing"  taggedAs OrgsTag in {
      cl.put[Json](s"/orgs/$id", create, Leela) {
        (json, response) =>
          response.status shouldEqual StatusCodes.Forbidden
          json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json", errorCtx)
      }.runSyncUnsafe()
    }

    "add orgs/create permissions for user"  taggedAs OrgsTag in {
      addPermission(
        s"/$id",
        Leela,
        testRealm,
        Organizations.Create
      ).runSyncUnsafe()
    }

    "create organization" taggedAs OrgsTag in {
      cl.put[Json](s"/orgs/$id", create, Leela) {
        (json, response) =>
          response.status shouldEqual StatusCodes.Created
          filterMetadataKeys(json) shouldEqual createRespJson(
            id,
            1L,
            "orgs",
            "Organization",
            Leela)
      }.runSyncUnsafe()
    }

    "fail when wrong revision is provided"  taggedAs OrgsTag in {
      cl.put[Json](s"/orgs/$id?rev=4", create, Leela) {
        (json, response) =>
          response.status shouldEqual StatusCodes.Conflict
          json shouldEqual jsonContentOf("/admin/errors/org-incorrect-revision.json", errorCtx)
      }.runSyncUnsafe()
    }

    val nonExistent = genId()
    "add orgs/write permissions for non-existing organization" taggedAs OrgsTag in {
      addPermission(
        s"/$nonExistent",
        Leela,
        testRealm,
        Organizations.Write
      ).runSyncUnsafe()
    }

    "fail when organization does not exist"  taggedAs OrgsTag in {
      cl.put[Json](s"/orgs/$nonExistent?rev=1", create, Leela) {
        (json, response) =>
          response.status shouldEqual StatusCodes.NotFound
          json shouldEqual jsonContentOf("/admin/errors/not-exists.json", Map(quote("{orgId}") -> nonExistent))
      }.runSyncUnsafe()
    }

    "succeed and fetch revisions"  taggedAs OrgsTag in {
      val updatedName = s"$id organization update 1"
      cl.put[Json](s"/orgs/$id?rev=1", orgPayload(updatedName), Leela) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        filterMetadataKeys(json) shouldEqual createRespJson(
          id,
          2L,
          "orgs",
          "Organization",
          Leela
        )
      }.runSyncUnsafe()

      val updatedName2 = s"$id organization update 2"
      cl.put[Json](s"/orgs/$id?rev=2", orgPayload(updatedName2), Leela) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        filterMetadataKeys(json) shouldEqual createRespJson(
          id,
          3L,
          "orgs",
          "Organization",
          Leela
        )
      }.runSyncUnsafe()

      cl.get[Json](s"/orgs/$id", Leela) { (lastVersion, response) =>
        runTask {
          response.status shouldEqual StatusCodes.OK
          admin.validate(lastVersion, "Organization", "orgs", id, updatedName2, 3L, id)
          cl.get[Json](s"/orgs/$id?rev=3", Leela) { (thirdVersion, response) =>
            response.status shouldEqual StatusCodes.OK
            thirdVersion shouldEqual lastVersion
          }
        }
      }.runSyncUnsafe()

      cl.get[Json](s"/orgs/$id?rev=2", Leela) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        admin.validate(json, "Organization", "orgs", id, updatedName, 2L, id)
      }.runSyncUnsafe()

      cl.get[Json](s"/orgs/$id?rev=1", Leela) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        admin.validate(json, "Organization", "orgs", id, s"$id organization", 1L, id)
      }.runSyncUnsafe()
    }
  }

  "deprecating an organization" should {
    val id = genId()
    val name = genString()

    "add orgs/create permissions for user" taggedAs OrgsTag in {
      addPermission(
        s"/$id",
        Leela,
        testRealm,
        Organizations.Create
      ).runSyncUnsafe()
    }

    "create the organization" taggedAs OrgsTag in {
      cl.put[Json](s"/orgs/$id", orgPayload(name), Leela) { (json, response) =>
        response.status shouldEqual StatusCodes.Created
        filterMetadataKeys(json) shouldEqual createRespJson(
          id,
          1L,
          "orgs",
          "Organization",
          Leela
        )
      }.runSyncUnsafe()
    }

    "fail when wrong revision is provided" taggedAs OrgsTag in {
      cl.delete[Json](s"/orgs/$id?rev=4", Leela) { (json, response) =>
        response.status shouldEqual StatusCodes.Conflict
        json shouldEqual jsonContentOf("/admin/errors/org-incorrect-revision.json")
      }.runSyncUnsafe()
    }

    "fail when revision is not provided"  taggedAs OrgsTag in {
      cl.delete[Json](s"/orgs/$id", Leela) { (json, response) =>
        response.status shouldEqual StatusCodes.BadRequest
        json shouldEqual jsonContentOf("/admin/errors/rev-not-provided.json")
      }.runSyncUnsafe()
    }

    "succeed if organization exists"  taggedAs OrgsTag in {
      cl.delete[Json](s"/orgs/$id?rev=1", Leela) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        filterMetadataKeys(json) shouldEqual createRespJson(
          id,
          2L,
          "orgs",
          "Organization",
          Leela,
          deprecated = true
        )
      }.runSyncUnsafe()
      cl.get[Json](s"/orgs/$id", Leela) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        admin.validate(json, "Organization", "orgs", id, name, 2L, id, true)
      }.runSyncUnsafe()
      cl.get[Json](s"/orgs/$id?rev=1", Leela) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        admin.validate(json, "Organization", "orgs", id, name, 1L, id)
      }.runSyncUnsafe()
    }
  }

  private[tests] def orgPayload(description: String = genString()): Json = {
    val rep = Map(quote("{description}") -> description)
    jsonContentOf("/admin/orgs/payload.json", rep)
  }

  private[tests] def createRespJson(id: String,
                                    rev: Long,
                                    tpe: String = "projects",
                                    `@type`: String = "Project",
                                    authenticated: Authenticated,
                                    deprecated: Boolean = false): Json = {
    val resp = prefixesConfig.coreContextMap ++ Map(
      quote("{id}")         -> id,
      quote("{type}")       -> tpe,
      quote("{@type}")      -> `@type`,
      quote("{rev}")        -> rev.toString,
      quote("{deltaBase}")  -> config.deltaUri.toString(),
      quote("{realm}")      -> testRealm,
      quote("{user}")       -> authenticated.name,
      quote("{orgId}")      -> id,
      quote("{deprecated}") -> deprecated.toString
    )
    jsonContentOf("/admin/response.json", resp)
  }
}
