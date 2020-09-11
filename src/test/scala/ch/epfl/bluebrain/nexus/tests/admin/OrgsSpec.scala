package ch.epfl.bluebrain.nexus.tests.admin

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.EitherValues
import ch.epfl.bluebrain.nexus.tests.DeltaHttpClient._
import ch.epfl.bluebrain.nexus.tests.Identity.UserCredentials
import ch.epfl.bluebrain.nexus.tests.Optics._
import ch.epfl.bluebrain.nexus.tests.Tags.OrgsTag
import ch.epfl.bluebrain.nexus.tests.{ExpectedResponse, Identity, NewBaseSpec, Realm}
import io.circe.Json
import monix.execution.Scheduler.Implicits.global
import org.scalatest.OptionValues

class OrgsSpec extends NewBaseSpec with OptionValues with EitherValues{

  private val testRealm   = Realm("orgs" + genString())
  private val testClient = Identity.ClientCredentials(genString(), genString(), testRealm)
  private val Fry = UserCredentials(genString(), genString(), testRealm)
  private val Leela = UserCredentials(genString(), genString(), testRealm)

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

  private val UnauthorizedAccess = ExpectedResponse(
    StatusCodes.Forbidden,
    jsonContentOf("/iam/errors/unauthorized-access.json")
  )

  private val OrganizationConflict = ExpectedResponse(
    StatusCodes.Conflict,
    jsonContentOf("/admin/errors/org-incorrect-revision.json")
  )

  "creating an organization" should {
    "fail if the permissions are missing" taggedAs OrgsTag in {
      adminDsl.createOrganization(
        genId(),
        "Description",
        Fry,
        Some(UnauthorizedAccess)
      ).runSyncUnsafe()
    }

    "add necessary permissions for user" taggedAs OrgsTag in {
      aclDsl.addPermission(
        "/",
        Fry,
        Organizations.Create
      ).runSyncUnsafe()
    }

    val id = genId()
    "succeed if payload is correct"  taggedAs OrgsTag in {
       adminDsl.createOrganization(
         id,
         "Description",
         Fry
       ).runSyncUnsafe()
    }

    "check if permissions have been created for user" taggedAs OrgsTag in {
      aclDsl.checkAdminAcls(s"/$id", Fry)
        .runSyncUnsafe()
    }

    "fail if organization already exists"  taggedAs OrgsTag in {
      val duplicate = genId()

      adminDsl.createOrganization(
        duplicate,
        "Description",
        Fry
      ).runSyncUnsafe()

      adminDsl.createOrganization(
        duplicate,
        "Description",
        Fry,
        Some(
          ExpectedResponse(
            StatusCodes.Conflict,
            jsonContentOf(
              "/admin/errors/org-already-exists.json",
              Map(quote("{orgId}") -> duplicate)
            )
          )
        )
      ).runSyncUnsafe()
    }
  }

  "fetching an organization" should {
    val id = genId()
    "fail if the permissions are missing"  taggedAs OrgsTag in {
      adminDsl.createOrganization(
        id,
        s"Description $id",
        Fry
      ).runSyncUnsafe()

      cl.get[Json](s"/orgs/$id", Leela) { (json, response) =>
        response.status shouldEqual StatusCodes.Forbidden
        json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json", errorCtx)
      }.runSyncUnsafe()
    }

    "add orgs/read permissions for user"  taggedAs OrgsTag in {
      aclDsl.addPermission(
        "/",
        Leela,
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
      aclDsl.addPermission(
        s"/$nonExistent",
        Leela,
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
    val description = s"$id organization"

    "fail if the permissions are missing"  taggedAs OrgsTag in {
      adminDsl.createOrganization(
        id,
        description,
        Leela,
        Some(UnauthorizedAccess)
      ).runSyncUnsafe()
    }

    "add orgs/create permissions for user"  taggedAs OrgsTag in {
      aclDsl.addPermission(
        s"/$id",
        Leela,
        Organizations.Create
      ).runSyncUnsafe()
    }

    "create organization" taggedAs OrgsTag in {
      adminDsl.createOrganization(
        id,
        description,
        Leela
      ).runSyncUnsafe()
    }

    "fail when wrong revision is provided"  taggedAs OrgsTag in {
      adminDsl.updateOrganization(
        id,
        description,
        Leela,
        4L,
        Some(OrganizationConflict)
      ).runSyncUnsafe()
    }

    val nonExistent = genId()
    "add orgs/write permissions for non-existing organization" taggedAs OrgsTag in {
      aclDsl.addPermission(
        s"/$nonExistent",
        Leela,
        Organizations.Write
      ).runSyncUnsafe()
    }

    "fail when organization does not exist"  taggedAs OrgsTag in {
      val notFound = ExpectedResponse(
        StatusCodes.NotFound,
        jsonContentOf("/admin/errors/not-exists.json", Map(quote("{orgId}") -> nonExistent))
      )
      adminDsl.updateOrganization(
        nonExistent,
        description,
        Leela,
        1L,
        Some(notFound)
      ).runSyncUnsafe()
    }

    "succeed and fetch revisions"  taggedAs OrgsTag in {
      val updatedName = s"$id organization update 1"
      adminDsl.updateOrganization(
        id,
        updatedName,
        Leela,
        1L
      ).runSyncUnsafe()

      val updatedName2 = s"$id organization update 2"
      adminDsl.updateOrganization(
        id,
        updatedName2,
        Leela,
        2L
      ).runSyncUnsafe()

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
      aclDsl.addPermission(
        s"/$id",
        Leela,
        Organizations.Create
      ).runSyncUnsafe()
    }

    "create the organization" taggedAs OrgsTag in {
      adminDsl.createOrganization(
        id,
        name,
        Leela
      ).runSyncUnsafe()
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
        filterMetadataKeys(json) shouldEqual adminDsl.createRespJson(
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
        admin.validate(json, "Organization", "orgs", id, name, 2L, id, deprecated = true)
      }.runSyncUnsafe()
      cl.get[Json](s"/orgs/$id?rev=1", Leela) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        admin.validate(json, "Organization", "orgs", id, name, 1L, id)
      }.runSyncUnsafe()
    }
  }
}
