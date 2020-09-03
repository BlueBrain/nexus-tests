package ch.epfl.bluebrain.nexus.tests.kg

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.model.{HttpEntity, HttpHeader, MediaTypes, StatusCodes, HttpRequest => Req}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.http.RdfMediaTypes
import ch.epfl.bluebrain.nexus.commons.test.EitherValues
import ch.epfl.bluebrain.nexus.rdf.syntax._
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import ch.epfl.bluebrain.nexus.tests.Tags.ToMigrateTag
import io.circe.Json
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, Inspectors}

import scala.collection.immutable.Seq

class ViewsSpec extends BaseSpec with Eventually with Inspectors with CancelAfterFailure with EitherValues {

  val orgId  = genId()
  val projId = genId()
  val fullId = s"$orgId/$projId"

  val projId2 = genId()
  val fullId2 = s"$orgId/$projId2"

  "creating projects" should {

    "add necessary permissions for user" taggedAs ToMigrateTag in {
      val json      = jsonContentOf("/iam/add.json", replSub + (quote("{perms}")       -> "organizations/create")).toEntity
      val jsonAnnon = jsonContentOf("/iam/add_annon.json", replSub + (quote("{perms}") -> "views/query")).toEntity

      cl(Req(PATCH, s"$iamBase/acls/$orgId", headersServiceAccount, json))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PATCH, s"$iamBase/acls/$fullId2", headersServiceAccount, jsonAnnon))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "succeed if payload is correct" taggedAs ToMigrateTag in {
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersJsonUser, orgReqEntity(orgId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PUT, s"$adminBase/projects/$fullId", headersJsonUser, kgProjectReqEntity(name = fullId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)

      cl(Req(PUT, s"$adminBase/projects/$fullId2", headersJsonUser, kgProjectReqEntity(name = fullId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }
  }

  "creating the view" should {

    "create a context" taggedAs ToMigrateTag in {
      val payload = jsonContentOf("/kg/views/context.json")
      forAll(List(fullId, fullId2)) { project =>
        cl(Req(PUT, s"$kgBase/resources/$project/resource/test-resource:context", headersJsonUser, payload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }

    "wait until in project resolver is created" taggedAs ToMigrateTag in {
      eventually {
        cl(Req(GET, s"$kgBase/resolvers/$fullId", headersJsonUser)).mapJson { (json, result) =>
          json.asObject.value("_total").value.asNumber.value.toInt.value shouldEqual 1
          result.status shouldEqual StatusCodes.OK
        }
      }
    }

    "create an ElasticSearch view" taggedAs ToMigrateTag in {

      val payload = jsonContentOf("/kg/views/elastic-view.json")

      forAll(List(fullId, fullId2)) { project =>
        eventually {
          cl(Req(PUT, s"$kgBase/views/$project/test-resource:testView", headersJsonUser, payload.toEntity))
            .mapResp(_.status shouldEqual StatusCodes.Created)
        }
      }
    }

    "create an Sparql view that index tags" in eventually {
      val payload = jsonContentOf("/kg/views/sparql-view.json")
      cl(Req(PUT, s"$kgBase/views/$fullId/test-resource:testSparqlView", headersJsonUser, payload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "get the created SparqlView" in eventually {
      cl(Req(GET, s"$kgBase/views/$fullId/test-resource:testSparqlView", headersJsonUser))
        .mapJson {
          case (json, _) =>
            val uuid = json.hcursor.get[String]("_uuid").rightValue
            val expectedMap = Map(
              quote("{id}")             -> "https://dev.nexus.test.com/simplified-resource/testSparqlView",
              quote("{resources}")      -> s"$kgBase/views/$fullId/test-resource:testSparqlView",
              quote("{project-parent}") -> s"$adminBase/projects/$fullId",
              quote("{iamBase}")        -> config.iam.uri.toString(),
              quote("{user}")           -> config.iam.testUserSub,
              quote("{uuid}")           -> uuid
            )
            val expected = jsonContentOf("/kg/views/sparql-view-response.json", expectedMap)
            json.removeKeys("_createdAt", "_updatedAt") should equalIgnoreArrayOrder(expected)
        }
    }

    "create an AggregateSparqlView" taggedAs ToMigrateTag in {

      val payload = jsonContentOf(
        "/kg/views/agg-sparql-view.json",
        Map(quote("{project1}") -> fullId, quote("{project2}") -> fullId2)
      )

      eventually {
        cl(Req(PUT, s"$kgBase/views/$fullId2/test-resource:testAggView", headersJsonUser, payload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }

    "create an AggregateElasticSearchView" taggedAs ToMigrateTag in {

      val payload = jsonContentOf(
        "/kg/views/agg-elastic-view.json",
        Map(quote("{project1}") -> fullId, quote("{project2}") -> fullId2)
      )

      eventually {
        cl(Req(PUT, s"$kgBase/views/$fullId2/test-resource:testAggEsView", headersJsonUser, payload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }

    "get the created AggregateElasticSearchView" in eventually {
      cl(Req(GET, s"$kgBase/views/$fullId2/test-resource:testAggEsView", headersJsonUser))
        .mapJson {
          case (json, _) =>
            val uuid = json.hcursor.get[String]("_uuid").rightValue
            val expectedMap = Map(
              quote("{id}")             -> "https://dev.nexus.test.com/simplified-resource/testAggEsView",
              quote("{resources}")      -> s"$kgBase/views/$fullId2/test-resource:testAggEsView",
              quote("{project-parent}") -> s"$adminBase/projects/$fullId2",
              quote("{project1}")       -> fullId,
              quote("{project2}")       -> fullId2,
              quote("{iamBase}")        -> config.iam.uri.toString(),
              quote("{user}")           -> config.iam.testUserSub,
              quote("{uuid}")           -> uuid
            )
            val expected = jsonContentOf("/kg/views/agg-elastic-view-response.json", expectedMap)
            json.removeKeys("_createdAt", "_updatedAt") should equalIgnoreArrayOrder(expected)
        }
    }

    "get an AggregateSparqlView" in eventually {
      cl(Req(GET, s"$kgBase/views/$fullId2/test-resource:testAggView", headersJsonUser))
        .mapJson {
          case (json, _) =>
            val uuid = json.hcursor.get[String]("_uuid").rightValue
            val expectedMap = Map(
              quote("{id}")             -> "https://dev.nexus.test.com/simplified-resource/testAggView",
              quote("{resources}")      -> s"$kgBase/views/$fullId2/test-resource:testAggView",
              quote("{project-parent}") -> s"$adminBase/projects/$fullId2",
              quote("{project1}")       -> fullId,
              quote("{project2}")       -> fullId2,
              quote("{iamBase}")        -> config.iam.uri.toString(),
              quote("{user}")           -> config.iam.testUserSub,
              quote("{uuid}")           -> uuid
            )
            val expected = jsonContentOf("/kg/views/agg-sparql-view-response.json", expectedMap)
            json.removeKeys("_createdAt", "_updatedAt") should equalIgnoreArrayOrder(expected)
        }
    }

    "post instances" taggedAs ToMigrateTag in {

      forAll(1 to 8) { i =>
        val payload      = jsonContentOf(s"/kg/views/instances/instance$i.json")
        val id           = payload.asObject.value("@id").value.asString.value
        val unprefixedId = id.stripPrefix("https://bbp.epfl.ch/nexus/v0/data/bbp/experiment/patchedcell/v0.1.0/")
        val projectId    = if (i > 5) fullId2 else fullId
        cl(
          Req(
            PUT,
            s"$kgBase/resources/$projectId/resource/patchedcell:$unprefixedId",
            headersJsonUser,
            payload.removeKeys("@id").toEntity
          )
        ).mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }

    "wait until in project view is indexed" taggedAs ToMigrateTag in {
      eventually {
        cl(Req(GET, s"$kgBase/views/$fullId", headersJsonUser)).mapJson { (json, result) =>
          json.asObject.value("_total").value.asNumber.value.toInt.value shouldEqual 4
          result.status shouldEqual StatusCodes.OK
        }
      }
    }

    "wait until all instances are indexed in default view of project 2" taggedAs ToMigrateTag in {
      eventually {
        cl(Req(GET, s"$kgBase/resources/$fullId2/resource", headersJsonUser)).mapJson { (json, result) =>
          json.asObject.value("_total").value.asNumber.value.toInt.value shouldEqual 4
          result.status shouldEqual StatusCodes.OK
        }
      }
    }

    "return 400 with bad query instances" taggedAs ToMigrateTag in {

      val query = Json.obj("query" -> Json.obj("other" -> Json.obj()))

      cl(Req(POST, s"$kgBase/views/$fullId/test-resource:testView/_search", headersJsonUser, query.toEntity))
        .mapJson { (json, result) =>
          json shouldEqual jsonContentOf("/kg/views/elastic-error.json")
          result.status shouldEqual StatusCodes.BadRequest
        }
    }

    val sort = Json.obj("sort" -> Json.arr(Json.obj("name.raw" -> Json.obj("order" -> Json.fromString("asc")))))

    val sortedMatchCells =
      Json.obj("query" -> Json.obj("term" -> Json.obj("@type" -> Json.fromString("Cell")))) deepMerge sort

    val matchAll = Json.obj("query" -> Json.obj("match_all" -> Json.obj())) deepMerge sort

    "search instances on project 1" in eventually {
      cl(Req(POST, s"$kgBase/views/$fullId/test-resource:testView/_search", headersJsonUser, sortedMatchCells.toEntity))
        .mapJson { (json, result) =>
          val index = json.getJson("hits").getArray("hits").head.getString("_index")
          result.status shouldEqual StatusCodes.OK
          json.removeKeys("took") shouldEqual jsonContentOf(
            "/kg/views/es-search-response.json",
            Map(quote("{index}") -> index)
          )
          cl(Req(POST, s"$kgBase/views/$fullId/test-resource:testView/_search", headersJsonUser, matchAll.toEntity))
            .mapJson { (json2, _) =>
              json2.removeKeys("took") shouldEqual json.removeKeys("took")
            }
        }
    }

    "search instances on project 2" in eventually {
      cl(
        Req(POST, s"$kgBase/views/$fullId2/test-resource:testView/_search", headersJsonUser, sortedMatchCells.toEntity)
      ).mapJson { (json, result) =>
        val index = json.getJson("hits").getArray("hits").head.getString("_index")
        result.status shouldEqual StatusCodes.OK
        json.removeKeys("took") shouldEqual jsonContentOf(
          "/kg/views/es-search-response-2.json",
          Map(quote("{index}") -> index)
        )

        cl(Req(POST, s"$kgBase/views/$fullId2/test-resource:testView/_search", headersJsonUser, matchAll.toEntity))
          .mapJson { (json2, _) =>
            json2.removeKeys("took") shouldEqual json.removeKeys("took")
          }
      }
    }

    "search instances on project AggregatedElasticSearchView when logged" in eventually {
      cl(
        Req(
          POST,
          s"$kgBase/views/$fullId2/test-resource:testAggEsView/_search",
          headersJsonUser,
          sortedMatchCells.toEntity
        )
      ).mapJson { (json, result) =>
        val indexes   = json.getJson("hits").getArray("hits").map(_.hcursor.get[String]("_index").rightValue)
        val toReplace = indexes.zipWithIndex.map { case (value, i) => quote(s"{index${i + 1}}") -> value }.toMap
        result.status shouldEqual StatusCodes.OK
        json.removeKeys("took") shouldEqual jsonContentOf("/kg/views/es-search-response-aggregated.json", toReplace)
      }
    }

    "search instances on project AggregatedElasticSearchView as anonymous" in eventually {
      cl(
        Req(
          POST,
          s"$kgBase/views/$fullId2/test-resource:testAggEsView/_search",
          Seq[HttpHeader](Accept(MediaTypes.`application/json`)),
          sortedMatchCells.toEntity
        )
      ).mapJson { (json, result) =>
        val index = json.getJson("hits").getArray("hits").head.getString("_index")
        result.status shouldEqual StatusCodes.OK
        json.removeKeys("took") shouldEqual jsonContentOf(
          "/kg/views/es-search-response-2.json",
          Map(quote("{index}") -> index)
        )
      }
    }

    "fetch statistics for testView" taggedAs ToMigrateTag in {
      cl(Req(GET, s"$kgBase/views/$fullId/test-resource:testView/statistics", headersJsonUser))
        .mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          val map = Map(
            quote("{total}")     -> "12",
            quote("{processed}") -> "12",
            quote("{evaluated}") -> "6",
            quote("{discarded}") -> "6",
            quote("{remaining}") -> "0"
          )
          val expected = jsonContentOf("/kg/views/statistics.json", map)
          json.removeNestedKeys("lastEventDateTime", "lastProcessedEventDateTime") shouldEqual expected
        }
    }

    "fetch statistics for defaultElasticSearchIndex" taggedAs ToMigrateTag in {
      cl(Req(GET, s"$kgBase/views/$fullId/nxv:defaultElasticSearchIndex/statistics", headersJsonUser))
        .mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          val map = Map(
            quote("{total}")     -> "12",
            quote("{processed}") -> "12",
            quote("{evaluated}") -> "12",
            quote("{discarded}") -> "0",
            quote("{remaining}") -> "0"
          )
          val expected = jsonContentOf("/kg/views/statistics.json", map)
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

    "search instances in SPARQL endpoint in project 1" in eventually {
      cl(
        Req(
          POST,
          s"$kgBase/views/$fullId/nxv:defaultSparqlIndex/sparql",
          headersJsonUser,
          HttpEntity(RdfMediaTypes.`application/sparql-query`, query)
        )
      ).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json shouldEqual jsonContentOf("/kg/views/sparql-search-response.json")
      }
    }

    "search instances in SPARQL endpoint in project 2" in eventually {
      cl(
        Req(
          POST,
          s"$kgBase/views/$fullId2/nxv:defaultSparqlIndex/sparql",
          headersJsonUser,
          HttpEntity(RdfMediaTypes.`application/sparql-query`, query)
        )
      ).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json shouldEqual jsonContentOf("/kg/views/sparql-search-response-2.json")
      }
    }

    "search instances in AggregateSparqlView when logged" taggedAs ToMigrateTag in {
      cl(
        Req(
          POST,
          s"$kgBase/views/$fullId2/test-resource:testAggView/sparql",
          headersJsonUser,
          HttpEntity(RdfMediaTypes.`application/sparql-query`, query)
        )
      ).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json should equalIgnoreArrayOrder(jsonContentOf("/kg/views/sparql-search-response-aggregated.json"))
      }
    }

    "search instances in AggregateSparqlView as anonymous" taggedAs ToMigrateTag in {
      cl(
        Req(
          POST,
          s"$kgBase/views/$fullId2/test-resource:testAggView/sparql",
          Seq[HttpHeader](Accept(MediaTypes.`application/json`)),
          HttpEntity(RdfMediaTypes.`application/sparql-query`, query)
        )
      ).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json shouldEqual jsonContentOf("/kg/views/sparql-search-response-2.json")
      }
    }

    "fetch statistics for defaultSparqlIndex" taggedAs ToMigrateTag in {
      cl(Req(GET, s"$kgBase/views/$fullId/nxv:defaultSparqlIndex/statistics", headersJsonUser))
        .mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          val map = Map(
            quote("{total}")     -> "12",
            quote("{processed}") -> "12",
            quote("{evaluated}") -> "12",
            quote("{discarded}") -> "0",
            quote("{remaining}") -> "0"
          )
          val expected = jsonContentOf("/kg/views/statistics.json", map)
          json.removeNestedKeys("lastEventDateTime", "lastProcessedEventDateTime") shouldEqual expected
        }
    }

    "search instances in SPARQL endpoint in project 1 with custom SparqlView" in eventually {
      cl(
        Req(
          POST,
          s"$kgBase/views/$fullId/test-resource:testSparqlView/sparql",
          headersJsonUser,
          HttpEntity(RdfMediaTypes.`application/sparql-query`, query)
        )
      ).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json shouldEqual jsonContentOf("/kg/views/sparql-search-response-empty.json")
      }
    }

    "tag resources resource" taggedAs ToMigrateTag in {

      forAll(1 to 5) { i =>
        val payload      = jsonContentOf(s"/kg/views/instances/instance$i.json")
        val id           = payload.asObject.value("@id").value.asString.value
        val unprefixedId = id.stripPrefix("https://bbp.epfl.ch/nexus/v0/data/bbp/experiment/patchedcell/v0.1.0/")
        cl(
          Req(
            POST,
            s"$kgBase/resources/$fullId/resource/patchedcell:$unprefixedId/tags?rev=1",
            headersJsonUser,
            Json.obj("rev" -> Json.fromLong(1L), "tag" -> Json.fromString("one")).toEntity
          )
        ).mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }

    "search instances in SPARQL endpoint in project 1 with custom SparqlView after tags added" in eventually {
      cl(
        Req(
          POST,
          s"$kgBase/views/$fullId/test-resource:testSparqlView/sparql",
          headersJsonUser,
          HttpEntity(RdfMediaTypes.`application/sparql-query`, query)
        )
      ).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json shouldEqual jsonContentOf("/kg/views/sparql-search-response.json")
      }
    }

    "remove @type on a resource" taggedAs ToMigrateTag in {

      val payload      = jsonContentOf("/kg/views/instances/instance1.json").removeKeys("@type")
      val id           = payload.asObject.value("@id").value.asString.value
      val unprefixedId = id.stripPrefix("https://bbp.epfl.ch/nexus/v0/data/bbp/experiment/patchedcell/v0.1.0/")
      cl(
        Req(
          PUT,
          s"$kgBase/resources/$fullId/_/patchedcell:$unprefixedId?rev=2",
          headersJsonUser,
          payload.removeKeys("@id").toEntity
        )
      ).mapResp(_.status shouldEqual StatusCodes.OK)
    }

    "search instances on project 1 after removed @type" in eventually {
      cl(Req(POST, s"$kgBase/views/$fullId/test-resource:testView/_search", headersJsonUser, sortedMatchCells.toEntity))
        .mapJson { (json, result) =>
          val index = json.getJson("hits").getArray("hits").head.getString("_index")
          result.status shouldEqual StatusCodes.OK
          json.removeKeys("took") shouldEqual jsonContentOf(
            "/kg/views/es-search-response-no-type.json",
            Map(quote("{index}") -> index)
          )
          cl(Req(POST, s"$kgBase/views/$fullId/test-resource:testView/_search", headersJsonUser, matchAll.toEntity))
            .mapJson { (json2, _) =>
              json2.removeKeys("took") shouldEqual json.removeKeys("took")
            }
        }
    }

    "deprecate a resource" taggedAs ToMigrateTag in {
      val payload      = jsonContentOf("/kg/views/instances/instance2.json").removeKeys("@type")
      val id           = payload.asObject.value("@id").value.asString.value
      val unprefixedId = id.stripPrefix("https://bbp.epfl.ch/nexus/v0/data/bbp/experiment/patchedcell/v0.1.0/")
      cl(Req(DELETE, s"$kgBase/resources/$fullId/_/patchedcell:$unprefixedId?rev=2", headersJsonUser))
        .mapResp(_.status shouldEqual StatusCodes.OK)
    }

    "search instances on project 1 after deprecated" in eventually {
      cl(Req(POST, s"$kgBase/views/$fullId/test-resource:testView/_search", headersJsonUser, sortedMatchCells.toEntity))
        .mapJson { (json, result) =>
          val index = json.getJson("hits").getArray("hits").head.getString("_index")
          result.status shouldEqual StatusCodes.OK
          json.removeKeys("took") shouldEqual jsonContentOf(
            "/kg/views/es-search-response-no-deprecated.json",
            Map(quote("{index}") -> index)
          )
          cl(Req(POST, s"$kgBase/views/$fullId/test-resource:testView/_search", headersJsonUser, matchAll.toEntity))
            .mapJson { (json2, _) =>
              json2.removeKeys("took") shouldEqual json.removeKeys("took")
            }
        }
    }
  }
}
