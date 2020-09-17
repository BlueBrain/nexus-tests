package ch.epfl.bluebrain.nexus.tests.admin

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import akka.stream.Materializer
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.{Randomness, Resources}
import ch.epfl.bluebrain.nexus.tests.HttpClientDsl._
import ch.epfl.bluebrain.nexus.tests.ExpectedResponse
import ch.epfl.bluebrain.nexus.tests.Identity.Authenticated
import ch.epfl.bluebrain.nexus.tests.Optics.filterMetadataKeys
import ch.epfl.bluebrain.nexus.tests.config.{PrefixesConfig, TestsConfig}
import com.typesafe.scalalogging.Logger
import io.circe.Json
import monix.bio.Task
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers

class AdminDsl(prefixesConfig: PrefixesConfig,
               config: TestsConfig)
              (implicit cl: UntypedHttpClient[Task],
               materializer: Materializer) extends Randomness with Resources with Matchers {

  private val logger = Logger[this.type]

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
      quote("{deltaUri}")  -> config.deltaUri.toString(),
      quote("{realm}")      -> authenticated.realm.name,
      quote("{user}")       -> authenticated.name,
      quote("{orgId}")      -> id,
      quote("{deprecated}") -> deprecated.toString
    )
    jsonContentOf("/admin/response.json", resp)
  }

  private def queryParams(revision: Long) = if(revision == 0L) {
    ""
  } else {
    s"?rev=$revision"
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
    cl.put[Json](s"/orgs/$id${queryParams(revision)}", orgPayload(description), authenticated) { (json, response) =>
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

  private[tests] val startPool = Vector.range('a', 'z')
  private[tests] val pool      = Vector.range('a', 'z') ++ Vector.range('0', '9') :+ '_' :+ '-'

  private[tests] def randomProjectPrefix = genString(1, startPool) + genString(genInt(10), pool)

  def projectPayload(path: String = "/admin/projects/create.json",
                     nxv: String = randomProjectPrefix,
                     person: String = randomProjectPrefix,
                     description: String = genString(),
                     base: String = s"${config.deltaUri.toString()}/${genString()}/",
                     vocab: String = s"${config.deltaUri.toString()}/${genString()}/"): Json = {
    val rep = Map(
      quote("{nxv-prefix}")    -> nxv,
      quote("{person-prefix}") -> person,
      quote("{description}")   -> description,
      quote("{base}")          -> base,
      quote("{vocab}")         -> vocab
    )
    jsonContentOf(path, rep)
  }

  def createProject(orgId: String,
                    projectId: String,
                    json: Json,
                    authenticated: Authenticated,
                    expectedResponse: Option[ExpectedResponse] = None): Task[Assertion] =
    updateProject(orgId, projectId, json, authenticated, 0L, expectedResponse)

  def updateProject(orgId: String,
                    projectId: String,
                    payload: Json,
                    authenticated: Authenticated,
                    revision: Long,
                    expectedResponse: Option[ExpectedResponse] = None): Task[Assertion] =
    cl.put[Json](s"/projects/$orgId/$projectId${queryParams(revision)}", payload, authenticated) { (json, response) =>
      logger.info(s"Creating/updating project $orgId/$projectId at revision $revision")
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
            s"$orgId/$projectId",
            revision + 1L,
            authenticated = authenticated
          )
      }

    }

}
