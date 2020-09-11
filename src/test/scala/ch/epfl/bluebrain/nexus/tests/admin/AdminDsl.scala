package ch.epfl.bluebrain.nexus.tests.admin

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import akka.stream.Materializer
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.{Randomness, Resources}
import ch.epfl.bluebrain.nexus.tests.DeltaHttpClient._
import ch.epfl.bluebrain.nexus.tests.ExpectedResponse
import ch.epfl.bluebrain.nexus.tests.Identity.Authenticated
import ch.epfl.bluebrain.nexus.tests.Optics.filterMetadataKeys
import ch.epfl.bluebrain.nexus.tests.config.{PrefixesConfig, TestsConfig}
import io.circe.Json
import monix.bio.Task
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers

class AdminDsl(prefixesConfig: PrefixesConfig,
               config: TestsConfig)
              (implicit cl: UntypedHttpClient[Task],
               materializer: Materializer) extends Randomness with Resources with Matchers {

  def orgPayload(description: String = genString()): Json = {
    val rep = Map(quote("{description}") -> description)
    jsonContentOf("/admin/orgs/payload.json", rep)
  }

  def createRespJson(id: String,
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
      quote("{realm}")      -> authenticated.realm.name,
      quote("{user}")       -> authenticated.name,
      quote("{orgId}")      -> id,
      quote("{deprecated}") -> deprecated.toString
    )
    jsonContentOf("/admin/response.json", resp)
  }

  def createOrganization(id: String,
                         description: String,
                         authenticated: Authenticated,
                         expectedResponse: Option[ExpectedResponse] = None): Task[Assertion] =
    updateOrganization(
      id,
      description,
      authenticated,
      0L,
      expectedResponse)

  def updateOrganization(id: String,
                         description: String,
                         authenticated: Authenticated,
                         revision: Long,
                         expectedResponse: Option[ExpectedResponse] = None): Task[Assertion] = {
    val queryParams = if(revision == 0L) {
      ""
    } else {
      s"?rev=$revision"
    }
    cl.put[Json](s"/orgs/$id$queryParams", orgPayload(description), authenticated) { (json, response) =>
      expectedResponse match {
        case Some(e) =>
          response.status shouldEqual e.statusCode
          json shouldEqual e.json
        case None =>
          if(revision == 0L)
            response.status shouldEqual StatusCodes.Created
          else
            response.status shouldEqual StatusCodes.OK

          filterMetadataKeys(json) shouldEqual createRespJson(
            id,
            revision + 1L,
            "orgs",
            "Organization",
            authenticated
          )
      }
    }
  }

}
