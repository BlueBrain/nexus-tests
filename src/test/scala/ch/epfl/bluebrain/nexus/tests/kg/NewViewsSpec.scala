package ch.epfl.bluebrain.nexus.tests.kg

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import cats.implicits._
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.{CirceEq, EitherValues}
import ch.epfl.bluebrain.nexus.tests.HttpClientDsl._
import ch.epfl.bluebrain.nexus.tests.Identity.UserCredentials
import ch.epfl.bluebrain.nexus.tests.Optics._
import ch.epfl.bluebrain.nexus.tests.Tags.ViewsTag
import ch.epfl.bluebrain.nexus.tests.iam.types.Permission.{Organizations, Views}
import ch.epfl.bluebrain.nexus.tests.{Identity, NewBaseSpec, Realm}
import io.circe.Json
import monix.execution.Scheduler.Implicits.global
import org.scalatest.concurrent.Eventually

class NewViewsSpec extends NewBaseSpec
  with Eventually
  with EitherValues
  with CirceEq {

  private val testRealm   = Realm("views" + genString())
  private val testClient = Identity.ClientCredentials(genString(), genString(), testRealm)
  private val ScoobyDoo = UserCredentials(genString(), genString(), testRealm)
  private val Shaggy = UserCredentials(genString(), genString(), testRealm)

  private val orgId  = genId()
  private val projId = genId()
  val fullId = s"$orgId/$projId"

  private val projId2 = genId()
  val fullId2 = s"$orgId/$projId2"

  val projects = List(fullId, fullId2)

  override def beforeAll(): Unit = {
    super.beforeAll()
    initRealm(
      testRealm,
      Identity.ServiceAccount,
      testClient,
      ScoobyDoo :: Shaggy :: Nil
    ).runSyncUnsafe()
  }

  "creating projects" should {
    "add necessary permissions for user" taggedAs ViewsTag in {
      {
        for {
          _ <- aclDsl.addPermission(s"/$orgId", ScoobyDoo, Organizations.Create)
          _ <- aclDsl.addPermissionAnonymous(s"/$fullId2", Views.Query)
        } yield {}
      }.runSyncUnsafe()
    }

    "succeed if payload is correct" taggedAs ViewsTag in {
      {
        for {
          _ <- adminDsl.createOrganization(orgId, orgId, ScoobyDoo)
          _ <- adminDsl.createProject(orgId, projId, kgDsl.projectJson(name = fullId), ScoobyDoo)
          _ <- adminDsl.createProject(orgId, projId2, kgDsl.projectJson(name = fullId2), ScoobyDoo)
        } yield {}
      }.runSyncUnsafe()
    }
  }

  "creating the view" should {
    "create a context" taggedAs ViewsTag in {
      val payload = jsonContentOf("/kg/views/context.json")

      projects.traverse { project =>
        cl.put[Json](s"/resources/$project/resource/test-resource:context", payload, ScoobyDoo) {
          (_, response) => response.status shouldEqual StatusCodes.Created
        }
      }.runSyncUnsafe()
    }

    "wait until in project resolver is created" taggedAs ViewsTag in {
      eventually {
        cl.get[Json](s"/resolvers/$fullId", ScoobyDoo) { (json, response) =>
          response.status shouldEqual StatusCodes.OK
          resources._total.getOption(json).value shouldEqual 1L
        }.runSyncUnsafe()
      }
    }

    "create an ElasticSearch view" taggedAs ViewsTag in {
      val payload = jsonContentOf("/kg/views/elastic-view.json")

      projects.traverse { project =>
        cl.put[Json](s"/views/$project/test-resource:testView", payload, ScoobyDoo) {
          (_, response) => response.status shouldEqual StatusCodes.Created
        }
      }.runSyncUnsafe()
    }

    "create an Sparql view that index tags" taggedAs ViewsTag in {
      val payload = jsonContentOf("/kg/views/sparql-view.json")
      cl.put[Json](s"/views/$fullId/test-resource:testSparqlView", payload, ScoobyDoo) {
        (_, response) => response.status shouldEqual StatusCodes.Created
      }.runSyncUnsafe()
    }

    "get the created SparqlView" taggedAs ViewsTag in {
      cl.get[Json](s"/views/$fullId/test-resource:testSparqlView", ScoobyDoo) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK

          val expected = jsonContentOf(
            "/kg/views/sparql-view-response.json",
            replacements(
              ScoobyDoo,
              quote("{id}")             -> "https://dev.nexus.test.com/simplified-resource/testSparqlView",
              quote("{resources}")      -> s"${config.deltaUri}/views/$fullId/test-resource:testSparqlView",
              quote("{project-parent}") -> s"${config.deltaUri}/projects/$fullId"
            )
          )

          filterMetadataKeys(json) should equalIgnoreArrayOrder(expected)
      }.runSyncUnsafe()
    }

    "create an AggregateSparqlView" taggedAs ViewsTag in {
      val payload = jsonContentOf(
        "/kg/views/agg-sparql-view.json",
        Map(quote("{project1}") -> fullId, quote("{project2}") -> fullId2)
      )

      cl.put[Json](s"/views/$fullId2/test-resource:testAggView", payload, ScoobyDoo) {
        (_, response) =>
          response.status shouldEqual StatusCodes.Created
      }.runSyncUnsafe()
    }

    "create an AggregateElasticSearchView" taggedAs ViewsTag in {
      val payload = jsonContentOf(
        "/kg/views/agg-elastic-view.json",
        Map(quote("{project1}") -> fullId, quote("{project2}") -> fullId2)
      )

      cl.put[Json](s"/views/$fullId2/test-resource:testAggEsView", payload, ScoobyDoo) {
        (_, response) =>
          response.status shouldEqual StatusCodes.Created
      }.runSyncUnsafe()
    }

    "get the created AggregateElasticSearchView" taggedAs ViewsTag in {
      cl.get[Json](s"/views/$fullId2/test-resource:testAggEsView", ScoobyDoo) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK

          val expected = jsonContentOf(
            "/kg/views/agg-elastic-view-response.json",
            replacements(
              ScoobyDoo,
              quote("{id}")             -> "https://dev.nexus.test.com/simplified-resource/testAggEsView",
              quote("{resources}")      -> s"${config.deltaUri}/views/$fullId2/test-resource:testAggEsView",
              quote("{project-parent}") -> s"${config.deltaUri}/projects/$fullId2",
              quote("{project1}")       -> fullId,
              quote("{project2}")       -> fullId2
            )
          )

          filterMetadataKeys(json) should equalIgnoreArrayOrder(expected)
      }.runSyncUnsafe()
    }
  }
}
