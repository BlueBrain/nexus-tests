package ch.epfl.bluebrain.nexus.tests.kg

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.model.{HttpEntity, HttpHeader, MediaTypes, StatusCodes, HttpRequest => Req}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.http.RdfMediaTypes
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import ch.epfl.bluebrain.nexus.tests.kg.types.ViewStatistics
import io.circe.Json
import org.scalatest.{CancelAfterFailure, EitherValues, Inspectors}
import org.scalatest.concurrent.Eventually
import scala.collection.immutable.Seq

class ViewsSpec extends BaseSpec with Eventually with Inspectors with CancelAfterFailure with EitherValues {

  val orgId  = genId()
  val projId = genId()
  val fullId = s"$orgId/$projId"

  val projId2 = genId()
  val fullId2 = s"$orgId/$projId2"

  "creating projects" should {

    "add necessary permissions for user" in {
      val json      = jsonContentOf("/iam/add.json", replSub + (quote("{perms}")       -> "organizations/create")).toEntity
      val jsonAnnon = jsonContentOf("/iam/add_annon.json", replSub + (quote("{perms}") -> "views/query")).toEntity

      cl(Req(PATCH, s"$iamBase/acls/$orgId", headersGroup, json)).mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PATCH, s"$iamBase/acls/$fullId2", headersGroup, jsonAnnon))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "succeed if payload is correct" in {
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersUserAcceptJson, orgReqEntity(orgId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PUT, s"$adminBase/projects/$fullId", headersUserAcceptJson, kgProjectReqEntity(name = fullId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)

      cl(Req(PUT, s"$adminBase/projects/$fullId2", headersUserAcceptJson, kgProjectReqEntity(name = fullId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }
  }

  "creating the view" should {

    "create an ElasticSearch view" in {

      val payload = jsonContentOf("/kg/views/elastic-view.json")

      forAll(List(fullId, fullId2)) { project =>
        eventually {
          cl(Req(PUT, s"$kgBase/views/$project/test-resource:testView", headersUserAcceptJson, payload.toEntity))
            .mapResp(_.status shouldEqual StatusCodes.Created)
        }
      }
    }

    "create an AggregateSparqlView" in {

      val payload = jsonContentOf("/kg/views/agg-sparql-view.json",
                                  Map(quote("{project1}") -> fullId, quote("{project2}") -> fullId2))

      eventually {
        cl(Req(PUT, s"$kgBase/views/$fullId2/test-resource:testAggView", headersUserAcceptJson, payload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }

    "create an AggregateElasticSearchView" in {

      val payload = jsonContentOf("/kg/views/agg-elastic-view.json",
                                  Map(quote("{project1}") -> fullId, quote("{project2}") -> fullId2))

      eventually {
        cl(Req(PUT, s"$kgBase/views/$fullId2/test-resource:testAggEsView", headersUserAcceptJson, payload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }

    "get an AggregateElasticSearchView" in eventually {
      cl(Req(GET, s"$kgBase/views/$fullId2/test-resource:testAggEsView", headersUserAcceptJson))
        .mapJson {
          case (json, _) =>
            val uuid = json.hcursor.get[String]("_uuid").right.value
            val expectedMap = Map(
              quote("{id}")             -> "https://dev.nexus.test.com/simplified-resource/testAggEsView",
              quote("{resources}")      -> s"$kgBase/views/$fullId2/test-resource:testAggEsView",
              quote("{project-parent}") -> s"$adminBase/projects/$fullId2",
              quote("{project1}")       -> fullId,
              quote("{project2}")       -> fullId2,
              quote("{iamBase}")        -> config.iam.uri.toString(),
              quote("{user}")           -> config.iam.userSub,
              quote("{uuid}")           -> uuid
            )
            val expected = jsonContentOf("/kg/views/agg-elastic-view-response.json", expectedMap)
            json.removeFields("_createdAt", "_updatedAt") should equalIgnoreArrayOrder(expected)
        }
    }

    "get an AggregateSparqlView" in eventually {
      cl(Req(GET, s"$kgBase/views/$fullId2/test-resource:testAggView", headersUserAcceptJson))
        .mapJson {
          case (json, _) =>
            val uuid = json.hcursor.get[String]("_uuid").right.value
            val expectedMap = Map(
              quote("{id}")             -> "https://dev.nexus.test.com/simplified-resource/testAggView",
              quote("{resources}")      -> s"$kgBase/views/$fullId2/test-resource:testAggView",
              quote("{project-parent}") -> s"$adminBase/projects/$fullId2",
              quote("{project1}")       -> fullId,
              quote("{project2}")       -> fullId2,
              quote("{iamBase}")        -> config.iam.uri.toString(),
              quote("{user}")           -> config.iam.userSub,
              quote("{uuid}")           -> uuid
            )
            val expected = jsonContentOf("/kg/views/agg-sparql-view-response.json", expectedMap)
            json.removeFields("_createdAt", "_updatedAt") should equalIgnoreArrayOrder(expected)
        }
    }

    "create a context" in {
      val payload = jsonContentOf("/kg/views/context.json")
      forAll(List(fullId, fullId2)) { project =>
        cl(
          Req(PUT,
              s"$kgBase/resources/$project/resource/test-resource:context",
              headersUserAcceptJson,
              payload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }

    }

    "wait until in project resolver is created" in {
      eventually {
        cl(Req(GET, s"$kgBase/resolvers/$fullId", headersUserAcceptJson)).mapJson { (json, result) =>
          json.asObject.value("_total").value.asNumber.value.toInt.value shouldEqual 1
          result.status shouldEqual StatusCodes.OK
        }
      }
    }
    "post instances" in {

      forAll(1 to 7) { i =>
        val payload      = jsonContentOf(s"/kg/views/instances/instance$i.json")
        val id           = payload.asObject.value("@id").value.asString.value
        val unprefixedId = id.stripPrefix("https://bbp.epfl.ch/nexus/v0/data/bbp/experiment/patchedcell/v0.1.0/")
        val projectId    = if (i > 5) fullId2 else fullId
        cl(
          Req(PUT,
              s"$kgBase/resources/$projectId/resource/patchedcell:$unprefixedId",
              headersUserAcceptJson,
              payload.removeField("@id").toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }

    "wait until in project view is indexed" in {
      eventually {
        cl(Req(GET, s"$kgBase/views/$fullId", headersUserAcceptJson)).mapJson { (json, result) =>
          json.asObject.value("_total").value.asNumber.value.toInt.value shouldEqual 3
          result.status shouldEqual StatusCodes.OK
        }
      }
    }

    "return 400 with bad query instances" in {

      val query = Json.obj("query" -> Json.obj("other" -> Json.obj()))

      cl(Req(POST, s"$kgBase/views/$fullId/test-resource:testView/_search", headersUserAcceptJson, query.toEntity))
        .mapJson { (json, result) =>
          json shouldEqual jsonContentOf("/kg/views/elastic-error.json")
          result.status shouldEqual StatusCodes.BadRequest
        }
    }

    val sortedMatchAll = Json.obj(
      "query" -> Json.obj(
        "term" -> Json.obj(
          "@type" -> Json.fromString("Cell")
        )),
      "sort" -> Json.arr(
        Json.obj(
          "name.raw" -> Json.obj(
            "order" -> Json.fromString("asc")
          ))
      )
    )

    "search instances on project 1" in eventually {
      cl(
        Req(POST,
            s"$kgBase/views/$fullId/test-resource:testView/_search",
            headersUserAcceptJson,
            sortedMatchAll.toEntity))
        .mapJson { (json, result) =>
          val index = json.getJson("hits").getArray("hits").head.getString("_index")
          result.status shouldEqual StatusCodes.OK
          json.removeField("took") shouldEqual jsonContentOf("/kg/views/es-search-response.json",
                                                             Map(quote("{index}") -> index))
        }
    }

    "search instances on project 2" in eventually {
      cl(
        Req(POST,
            s"$kgBase/views/$fullId2/test-resource:testView/_search",
            headersUserAcceptJson,
            sortedMatchAll.toEntity))
        .mapJson { (json, result) =>
          val index = json.getJson("hits").getArray("hits").head.getString("_index")
          result.status shouldEqual StatusCodes.OK
          json.removeField("took") shouldEqual jsonContentOf("/kg/views/es-search-response-2.json",
                                                             Map(quote("{index}") -> index))
        }
    }

    "search instances on project AggregatedElasticSearchView when logged" in eventually {
      cl(
        Req(POST,
            s"$kgBase/views/$fullId2/test-resource:testAggEsView/_search",
            headersUserAcceptJson,
            sortedMatchAll.toEntity))
        .mapJson { (json, result) =>
          val indexes   = json.getJson("hits").getArray("hits").map(_.hcursor.get[String]("_index").right.value)
          val toReplace = indexes.zipWithIndex.map { case (value, i) => quote(s"{index${i + 1}}") -> value }.toMap
          result.status shouldEqual StatusCodes.OK
          json.removeField("took") shouldEqual jsonContentOf("/kg/views/es-search-response-aggregated.json", toReplace)
        }
    }

    "search instances on project AggregatedElasticSearchView as anonymous" in eventually {
      cl(
        Req(POST,
            s"$kgBase/views/$fullId2/test-resource:testAggEsView/_search",
            Seq[HttpHeader](Accept(MediaTypes.`application/json`)),
            sortedMatchAll.toEntity))
        .mapJson { (json, result) =>
          val index = json.getJson("hits").getArray("hits").head.getString("_index")
          result.status shouldEqual StatusCodes.OK
          json.removeField("took") shouldEqual jsonContentOf("/kg/views/es-search-response-2.json",
                                                             Map(quote("{index}") -> index))
        }
    }

    "fetch statistics for testView" in {
      cl(Req(GET, s"$kgBase/views/$fullId/test-resource:testView/statistics", headersUserAcceptJson))
        .mapDecoded[ViewStatistics] { (stats, result) =>
          result.status shouldEqual StatusCodes.OK
          stats.delayInSeconds shouldEqual 0
          stats.remainingEvents shouldEqual 0
          stats.lastEventDateTime shouldEqual stats.lastEventDateTime
          stats.totalEvents shouldEqual 11
          stats.processedEvents shouldEqual 11
          stats.evaluatedEvents shouldEqual 6
          stats.discardedEvents shouldEqual 5
        }
    }

    "fetch statistics for defaultElasticSearchIndex" in {
      cl(Req(GET, s"$kgBase/views/$fullId/nxv:defaultElasticSearchIndex/statistics", headersUserAcceptJson))
        .mapDecoded[ViewStatistics] { (stats, result) =>
          result.status shouldEqual StatusCodes.OK
          stats.delayInSeconds shouldEqual 0
          stats.remainingEvents shouldEqual 0
          stats.lastEventDateTime shouldEqual stats.lastEventDateTime
          stats.totalEvents shouldEqual 11
          stats.processedEvents shouldEqual 11
          stats.evaluatedEvents shouldEqual 11
          stats.discardedEvents shouldEqual 0
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
        Req(POST,
            s"$kgBase/views/$fullId/nxv:defaultSparqlIndex/sparql",
            headersUserAcceptJson,
            HttpEntity(RdfMediaTypes.`application/sparql-query`, query)))
        .mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual jsonContentOf("/kg/views/sparql-search-response.json")
        }
    }

    "search instances in SPARQL endpoint in project 2" in eventually {
      cl(
        Req(POST,
            s"$kgBase/views/$fullId2/nxv:defaultSparqlIndex/sparql",
            headersUserAcceptJson,
            HttpEntity(RdfMediaTypes.`application/sparql-query`, query)))
        .mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual jsonContentOf("/kg/views/sparql-search-response-2.json")
        }
    }

    "search instances in AggregateSparqlView when logged" in {
      cl(
        Req(POST,
            s"$kgBase/views/$fullId2/test-resource:testAggView/sparql",
            headersUserAcceptJson,
            HttpEntity(RdfMediaTypes.`application/sparql-query`, query)))
        .mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json should equalIgnoreArrayOrder(jsonContentOf("/kg/views/sparql-search-response-aggregated.json"))
        }
    }

    "search instances in AggregateSparqlView as anonymous" in {
      cl(
        Req(
          POST,
          s"$kgBase/views/$fullId2/test-resource:testAggView/sparql",
          Seq[HttpHeader](Accept(MediaTypes.`application/json`)),
          HttpEntity(RdfMediaTypes.`application/sparql-query`, query)
        ))
        .mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual jsonContentOf("/kg/views/sparql-search-response-2.json")
        }
    }

    "fetch statistics for defaultSparqlIndex" in {
      cl(Req(GET, s"$kgBase/views/$fullId/nxv:defaultSparqlIndex/statistics", headersUserAcceptJson))
        .mapDecoded[ViewStatistics] { (stats, result) =>
          result.status shouldEqual StatusCodes.OK
          stats.delayInSeconds shouldEqual 0
          stats.remainingEvents shouldEqual 0
          stats.lastEventDateTime shouldEqual stats.lastEventDateTime
          stats.totalEvents shouldEqual 11
          stats.processedEvents shouldEqual 11
          stats.evaluatedEvents shouldEqual 11
          stats.discardedEvents shouldEqual 0
        }
    }

  }
}
