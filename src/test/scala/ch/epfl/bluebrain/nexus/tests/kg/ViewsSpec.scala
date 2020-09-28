package ch.epfl.bluebrain.nexus.tests.kg

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import cats.implicits._
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.{CirceEq, EitherValues}
import ch.epfl.bluebrain.nexus.rdf.syntax._
import ch.epfl.bluebrain.nexus.tests.HttpClientDsl._
import ch.epfl.bluebrain.nexus.tests.Identity.{Anonymous, UserCredentials}
import ch.epfl.bluebrain.nexus.tests.Optics._
import ch.epfl.bluebrain.nexus.tests.Tags.ViewsTag
import ch.epfl.bluebrain.nexus.tests.iam.types.Permission.{Organizations, Views}
import ch.epfl.bluebrain.nexus.tests.{Identity, BaseSpec, Realm}
import io.circe.Json
import monix.execution.Scheduler.Implicits.global

class ViewsSpec extends BaseSpec
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
      for {
        _ <- aclDsl.addPermission(s"/$orgId", ScoobyDoo, Organizations.Create)
        _ <- aclDsl.addPermissionAnonymous(s"/$fullId2", Views.Query)
      } yield succeed
    }

    "succeed if payload is correct" taggedAs ViewsTag in {
      for {
        _ <- adminDsl.createOrganization(orgId, orgId, ScoobyDoo)
        _ <- adminDsl.createProject(orgId, projId, kgDsl.projectJson(name = fullId), ScoobyDoo)
        _ <- adminDsl.createProject(orgId, projId2, kgDsl.projectJson(name = fullId2), ScoobyDoo)
      } yield succeed
    }
  }

  "creating the view" should {
    "create a context" taggedAs ViewsTag in {
      val payload = jsonContentOf("/kg/views/context.json")

      projects.traverse { project =>
        cl.put[Json](s"/resources/$project/resource/test-resource:context", payload, ScoobyDoo) {
          (_, response) => response.status shouldEqual StatusCodes.Created
        }
      }
    }

    "wait until in project resolver is created" taggedAs ViewsTag in {
      eventually {
        cl.get[Json](s"/resolvers/$fullId", ScoobyDoo) { (json, response) =>
          response.status shouldEqual StatusCodes.OK
          _total.getOption(json).value shouldEqual 1L
        }
      }
    }

    "create an ElasticSearch view" taggedAs ViewsTag in {
      val payload = jsonContentOf("/kg/views/elastic-view.json")

      projects.traverse { project =>
        cl.put[Json](s"/views/$project/test-resource:testView", payload, ScoobyDoo) {
          (_, response) => response.status shouldEqual StatusCodes.Created
        }
      }
    }

    "create an Sparql view that index tags" taggedAs ViewsTag in {
      val payload = jsonContentOf("/kg/views/sparql-view.json")
      cl.put[Json](s"/views/$fullId/test-resource:testSparqlView", payload, ScoobyDoo) {
        (_, response) => response.status shouldEqual StatusCodes.Created
      }
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
      }
    }

    "create an AggregateSparqlView" taggedAs ViewsTag in {
      val payload = jsonContentOf(
        "/kg/views/agg-sparql-view.json",
        Map(quote("{project1}") -> fullId, quote("{project2}") -> fullId2)
      )

      cl.put[Json](s"/views/$fullId2/test-resource:testAggView", payload, ScoobyDoo) {
        (_, response) =>
          response.status shouldEqual StatusCodes.Created
      }
    }

    "create an AggregateElasticSearchView" taggedAs ViewsTag in {
      val payload = jsonContentOf(
        "/kg/views/agg-elastic-view.json",
        Map(quote("{project1}") -> fullId, quote("{project2}") -> fullId2)
      )

      cl.put[Json](s"/views/$fullId2/test-resource:testAggEsView", payload, ScoobyDoo) {
        (_, response) =>
          response.status shouldEqual StatusCodes.Created
      }
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
      }
    }

    "get an AggregateSparqlView" taggedAs ViewsTag in {
      cl.get[Json](s"/views/$fullId2/test-resource:testAggView", ScoobyDoo) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK
          val expected = jsonContentOf(
            "/kg/views/agg-sparql-view-response.json",
            replacements(
              ScoobyDoo,
              quote("{id}")             -> "https://dev.nexus.test.com/simplified-resource/testAggView",
              quote("{resources}")      -> s"${config.deltaUri}/views/$fullId2/test-resource:testAggView",
              quote("{project-parent}") -> s"${config.deltaUri}/projects/$fullId2",
              quote("{project1}")       -> fullId,
              quote("{project2}")       -> fullId2
            )
          )

          filterMetadataKeys(json) should equalIgnoreArrayOrder(expected)
      }
    }

    "post instances" taggedAs ViewsTag in {
      (1 to 8).toList.traverse { i =>
        val payload      = jsonContentOf(s"/kg/views/instances/instance$i.json")
        val id = `@id`.getOption(payload).value
        val unprefixedId = id.stripPrefix("https://bbp.epfl.ch/nexus/v0/data/bbp/experiment/patchedcell/v0.1.0/")
        val projectId    = if (i > 5) fullId2 else fullId

        cl.put[Json](
          s"/resources/$projectId/resource/patchedcell:$unprefixedId",
          payload,
          ScoobyDoo
        ) { (_, response) =>
          response.status shouldEqual StatusCodes.Created
        }
      }
    }

    "wait until in project view is indexed" taggedAs ViewsTag in eventually {
      cl.get[Json](s"/views/$fullId", ScoobyDoo) { (json, response) =>
        _total.getOption(json).value shouldEqual 4
        response.status shouldEqual StatusCodes.OK
      }
    }

    "wait until all instances are indexed in default view of project 2" taggedAs ViewsTag in eventually {
      cl.get[Json](s"/resources/$fullId2/resource", ScoobyDoo) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        _total.getOption(json).value shouldEqual 4
      }
    }

    "return 400 with bad query instances" taggedAs ViewsTag in {
      val query = Json.obj("query" -> Json.obj("other" -> Json.obj()))
      cl.post[Json](s"/views/$fullId/test-resource:testView/_search", query, ScoobyDoo) {
        (json, response) =>
          response.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/kg/views/elastic-error.json")
      }
    }

    val sort = Json.obj("sort" -> Json.arr(Json.obj("name.raw" -> Json.obj("order" -> Json.fromString("asc")))))

    val sortedMatchCells =
      Json.obj("query" -> Json.obj("term" -> Json.obj("@type" -> Json.fromString("Cell")))) deepMerge sort

    val matchAll = Json.obj("query" -> Json.obj("match_all" -> Json.obj())) deepMerge sort

    "search instances on project 1" taggedAs ViewsTag in eventually {
      cl.post[Json](s"/views/$fullId/test-resource:testView/_search", sortedMatchCells, ScoobyDoo) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK
          val index = hits(0)._index.string.getOption(json).value
          filterKey("took")(json) shouldEqual jsonContentOf(
            "/kg/views/es-search-response.json",
            Map(quote("{index}") -> index)
          )

          cl.post[Json](s"/views/$fullId/test-resource:testView/_search", matchAll, ScoobyDoo) {
            (json2, _) =>
              filterKey("took")(json2) shouldEqual filterKey("took")(json)
          }.runSyncUnsafe()
      }
    }

    "search instances on project 2" taggedAs ViewsTag in eventually {
      cl.post[Json](s"/views/$fullId2/test-resource:testView/_search", sortedMatchCells, ScoobyDoo) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK
          val index = hits(0)._index.string.getOption(json).value
          filterKey("took")(json) shouldEqual jsonContentOf(
            "/kg/views/es-search-response-2.json",
            Map(quote("{index}") -> index)
          )

          cl.post[Json](s"/views/$fullId2/test-resource:testView/_search", matchAll, ScoobyDoo) {
            (json2, _) =>
              filterKey("took")(json2) shouldEqual filterKey("took")(json)
          }.runSyncUnsafe()
      }
    }

    "search instances on project AggregatedElasticSearchView when logged" taggedAs ViewsTag in eventually {
      cl.post[Json](
        s"/views/$fullId2/test-resource:testAggEsView/_search",
        sortedMatchCells,
        ScoobyDoo
      ) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        val indexes = hits.each._index.string.getAll(json)
        val toReplace = indexes.zipWithIndex.map { case (value, i) => quote(s"{index${i + 1}}") -> value }.toMap
        filterKey("took")(json) shouldEqual jsonContentOf("/kg/views/es-search-response-aggregated.json", toReplace)
      }
    }

    "search instances on project AggregatedElasticSearchView as anonymous" taggedAs ViewsTag in eventually {
      cl.post[Json](s"/views/$fullId2/test-resource:testAggEsView/_search", sortedMatchCells, Anonymous) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK
          val index = hits(0)._index.string.getOption(json).value
          filterKey("took")(json) shouldEqual jsonContentOf(
            "/kg/views/es-search-response-2.json",
            Map(quote("{index}") -> index)
          )
      }
    }

    "fetch statistics for testView" taggedAs ViewsTag in {
      cl.get[Json](s"/views/$fullId/test-resource:testView/statistics", ScoobyDoo) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK
          val expected = jsonContentOf(
            "/kg/views/statistics.json",
            Map(
              quote("{total}")     -> "12",
              quote("{processed}") -> "12",
              quote("{evaluated}") -> "6",
              quote("{discarded}") -> "6",
              quote("{remaining}") -> "0"
            )
          )
          json.removeNestedKeys("lastEventDateTime", "lastProcessedEventDateTime") shouldEqual expected
      }
    }

    val query =
      """
        |prefix nsg: <https://bbp-nexus.epfl.ch/vocabs/bbp/neurosciencegraph/core/v0.1.0/>
        |
        |select ?s where {
        |  ?s nsg:brainLocation / nsg:brainRegion <http://www.parcellation.org/0000013>
        |}
        |order by ?s
      """.stripMargin

    "search instances in SPARQL endpoint in project 1" taggedAs ViewsTag in {
      cl.sparqlQuery[Json](s"/views/$fullId/nxv:defaultSparqlIndex/sparql", query, ScoobyDoo) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK
          json shouldEqual jsonContentOf("/kg/views/sparql-search-response.json")
      }
    }

    "search instances in SPARQL endpoint in project 2" taggedAs ViewsTag in {
      cl.sparqlQuery[Json](s"/views/$fullId2/nxv:defaultSparqlIndex/sparql", query, ScoobyDoo) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK
          json shouldEqual jsonContentOf("/kg/views/sparql-search-response-2.json")
      }
    }

    "search instances in AggregateSparqlView when logged" taggedAs ViewsTag in {
      cl.sparqlQuery[Json](s"/views/$fullId2/test-resource:testAggView/sparql", query, ScoobyDoo) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK
          json should equalIgnoreArrayOrder(jsonContentOf("/kg/views/sparql-search-response-aggregated.json"))
      }
    }

    "search instances in AggregateSparqlView as anonymous" taggedAs ViewsTag in {
      cl.sparqlQuery[Json](s"/views/$fullId2/test-resource:testAggView/sparql", query, Anonymous) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK
          json should equalIgnoreArrayOrder(jsonContentOf("/kg/views/sparql-search-response-2.json"))
      }
    }

    "fetch statistics for defaultSparqlIndex" taggedAs ViewsTag in {
      cl.get[Json](s"/views/$fullId/nxv:defaultSparqlIndex/statistics", ScoobyDoo) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK
          val expected = jsonContentOf(
            "/kg/views/statistics.json",
            Map(
              quote("{total}")     -> "12",
              quote("{processed}") -> "12",
              quote("{evaluated}") -> "12",
              quote("{discarded}") -> "0",
              quote("{remaining}") -> "0"
            )
          )
          json.removeNestedKeys("lastEventDateTime", "lastProcessedEventDateTime") shouldEqual expected
      }
    }

    "search instances in SPARQL endpoint in project 1 with custom SparqlView" taggedAs ViewsTag in {
      cl.sparqlQuery[Json](s"/views/$fullId/test-resource:testSparqlView/sparql", query, ScoobyDoo) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK
          json shouldEqual jsonContentOf("/kg/views/sparql-search-response-empty.json")
      }
    }

    "tag resources resource" taggedAs ViewsTag in {
      (1 to 5).toList.traverse { i =>
        val payload      = jsonContentOf(s"/kg/views/instances/instance$i.json")
        val id           = `@id`.getOption(payload).value
        val unprefixedId = id.stripPrefix("https://bbp.epfl.ch/nexus/v0/data/bbp/experiment/patchedcell/v0.1.0/")
        cl.post[Json](
          s"/resources/$fullId/resource/patchedcell:$unprefixedId/tags?rev=1",
          Json.obj("rev" -> Json.fromLong(1L), "tag" -> Json.fromString("one")),
          ScoobyDoo
        ) {
          (_, response) => response.status shouldEqual StatusCodes.Created
        }
      }
    }

    "search instances in SPARQL endpoint in project 1 with custom SparqlView after tags added" taggedAs ViewsTag in {
      eventually {
        cl.sparqlQuery[Json](s"/views/$fullId/test-resource:testSparqlView/sparql", query, ScoobyDoo) {
          (json, response) =>
            response.status shouldEqual StatusCodes.OK
            json shouldEqual jsonContentOf("/kg/views/sparql-search-response.json")
        }
      }
    }

    "remove @type on a resource" taggedAs ViewsTag in {
      val payload      = jsonContentOf("/kg/views/instances/instance1.json").removeKeys("@type")
      val id           =`@id`.getOption(payload).value
      val unprefixedId = id.stripPrefix("https://bbp.epfl.ch/nexus/v0/data/bbp/experiment/patchedcell/v0.1.0/")

      cl.put[Json](
        s"/resources/$fullId/_/patchedcell:$unprefixedId?rev=2",
        filterKey("@id")(payload),
        ScoobyDoo
      ) {
        (_, response) => response.status shouldEqual StatusCodes.OK
      }
    }

    "search instances on project 1 after removed @type" taggedAs ViewsTag in  eventually {
      cl.post[Json](s"/views/$fullId/test-resource:testView/_search", sortedMatchCells, ScoobyDoo) {
        (json, response) =>
          response.status shouldEqual StatusCodes.OK
          val index = hits(0)._index.string.getOption(json).value
          filterKey("took")(json) shouldEqual jsonContentOf(
            "/kg/views/es-search-response-no-type.json",
            Map(quote("{index}") -> index)
          )

          cl.post[Json](s"/views/$fullId/test-resource:testView/_search", matchAll, ScoobyDoo) {
            (json2, _) =>
              filterKey("took")(json2) shouldEqual filterKey("took")(json)
          }.runSyncUnsafe()
      }
    }

    "deprecate a resource" taggedAs ViewsTag in {
      val payload      = jsonContentOf("/kg/views/instances/instance2.json").removeKeys("@type")
      val id           = payload.asObject.value("@id").value.asString.value
      val unprefixedId = id.stripPrefix("https://bbp.epfl.ch/nexus/v0/data/bbp/experiment/patchedcell/v0.1.0/")
      cl.delete[Json](s"/resources/$fullId/_/patchedcell:$unprefixedId?rev=2", ScoobyDoo) {
        (_, response) => response.status shouldEqual StatusCodes.OK
      }
    }

    "search instances on project 1 after deprecated" taggedAs ViewsTag in eventually {
      cl.post[Json](s"/views/$fullId/test-resource:testView/_search", sortedMatchCells, ScoobyDoo) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          val index = hits(0)._index.string.getOption(json).value
          filterKey("took")(json) shouldEqual jsonContentOf(
            "/kg/views/es-search-response-no-deprecated.json",
            Map(quote("{index}") -> index)
          )

          cl.post[Json](s"/views/$fullId/test-resource:testView/_search", matchAll, ScoobyDoo) {
            (json2, _) =>
              filterKey("took")(json2) shouldEqual filterKey("took")(json)
          }.runSyncUnsafe()
      }
    }
  }
}
