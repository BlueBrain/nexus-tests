package ch.epfl.bluebrain.nexus.tests.kg

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.{HttpEntity, StatusCodes, HttpRequest => Req}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.http.RdfMediaTypes
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import ch.epfl.bluebrain.nexus.tests.iam.types.AclListing
import io.circe.Json
import org.scalatest.{CancelAfterFailure, Inspectors}
import org.scalatest.concurrent.Eventually

class ViewsSpec extends BaseSpec with Eventually with Inspectors with CancelAfterFailure {

  val orgId  = genId()
  val projId = genId()
  val fullId = s"$orgId/$projId"

  "creating projects" should {

    "add necessary permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "organizations/create")
      ).toEntity
      cl(Req(GET, s"$iamBase/acls/", headersGroup)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        val rev = acls._results.head._rev

        cl(Req(PATCH, s"$iamBase/acls/?rev=$rev", headersGroup, json)).mapResp(_.status shouldEqual StatusCodes.OK)
      }
    }

    "succeed if payload is correct" in {
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersUserAcceptJson, orgReqEntity(orgId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PUT, s"$adminBase/projects/$fullId", headersUserAcceptJson, kgProjectReqEntity(name = fullId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }
  }

  "creating the view" should {
    "create an ElasticSearch view" in {

      val payload = jsonContentOf("/kg/views/elastic-view.json")

      eventually {
        cl(Req(PUT, s"$kgBase/views/$fullId/test-resource:testView", headersUserAcceptJson, payload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }

    "create a context" in {
      val payload = jsonContentOf("/kg/views/context.json")
      cl(Req(PUT, s"$kgBase/resources/$fullId/resource/test-resource:context", headersUserAcceptJson, payload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)
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

      forAll(1 to 5) { i =>
        val payload = jsonContentOf(s"/kg/views/instances/instance$i.json")
        val id = payload.asObject
          .value("@id")
          .value
          .asString
          .value
          .stripPrefix("https://bbp.epfl.ch/nexus/v0/data/bbp/experiment/patchedcell/v0.1.0/")
        cl(
          Req(PUT,
              s"$kgBase/resources/$fullId/resource/patchedcell:$id",
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

    "search instances" in {

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
      eventually {
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
    }

    "search instances in SPARQL endpoint" in {
      val query =
        """
          |prefix nsg: <https://bbp-nexus.epfl.ch/vocabs/bbp/neurosciencegraph/core/v0.1.0/>
          |
          |select ?s where {
          |  ?s nsg:brainLocation / nsg:brainRegion <http://www.parcellation.org/0000013>
          |}
          |order by ?s
        """.stripMargin
      eventually {
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
    }
  }
}
