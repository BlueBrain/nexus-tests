package ch.epfl.bluebrain.nexus.tests.kg

import java.net.URLEncoder
import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.{StatusCodes, HttpRequest => Req}
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.scaladsl.{Sink, Source}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import ch.epfl.bluebrain.nexus.tests.iam.types.AclListing
import io.circe.Json
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, EitherValues, Inspectors}

class ResourcesSpec extends BaseSpec with Eventually with Inspectors with CancelAfterFailure with EitherValues {

  val orgId   = genId()
  val projId1 = genId()
  val projId2 = genId()
  val id1     = s"$orgId/$projId1"
  val id2     = s"$orgId/$projId2"

  "fetching information" should {
    "return the software version" in {
      cl(Req(GET, config.kg.version)).mapJson { (json, result) =>
        json.asObject.value.keys.toSet shouldEqual Set("storage", "kg", "iam", "admin", "elasticsearch", "blazegraph")
        result.status shouldEqual StatusCodes.OK
      }
    }

    "return the cassandra and cluster status" in {
      cl(Req(GET, config.kg.status)).mapJson { (json, result) =>
        json shouldEqual Json.obj("cassandra" -> Json.fromString("up"), "cluster" -> Json.fromString("up"))
        result.status shouldEqual StatusCodes.OK
      }
    }
  }

  "creating projects" should {

    "add necessary permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "organizations/create")
      ).toEntity
      cl(Req(GET, s"$iamBase/acls/", headersServiceAccount)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        val rev = acls._results.head._rev

        cl(Req(PATCH, s"$iamBase/acls/?rev=$rev", headersServiceAccount, json))
          .mapResp(_.status shouldEqual StatusCodes.OK)
      }
    }

    "succeed if payload is correct" in {
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersJsonUser, orgReqEntity(orgId)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PUT, s"$adminBase/projects/$id1", headersJsonUser, kgProjectReqEntity(name = id1)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PUT, s"$adminBase/projects/$id2", headersJsonUser, kgProjectReqEntity(name = id2)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }
  }

  "adding schema" should {
    "create a schema" in {
      val schemaPayload = jsonContentOf("/kg/schemas/simple-schema.json")

      eventually {
        cl(Req(PUT, s"$kgBase/schemas/$id1/test-schema", headersJsonUser, schemaPayload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }
    "creating a schema with property shape" in {
      val schemaPayload = jsonContentOf("/kg/schemas/simple-schema-prop-shape.json")

      cl(Req(POST, s"$kgBase/schemas/$id1", headersJsonUser, schemaPayload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "creating a schema that imports the property shape schema" in {
      val schemaPayload = jsonContentOf("/kg/schemas/simple-schema-imports.json")

      eventually {
        cl(Req(POST, s"$kgBase/schemas/$id1", headersJsonUser, schemaPayload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }
  }

  "creating a resource" should {
    "succeed if the payload is correct" in {
      val payload =
        jsonContentOf(
          "/kg/resources/simple-resource.json",
          Map(quote("{priority}") -> "5", quote("{resourceId}") -> "1")
        )

      cl(Req(PUT, s"$kgBase/resources/$id1/test-schema/test-resource:1", headersJsonUser, payload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)

      val id2 = URLEncoder.encode("https://dev.nexus.test.com/test-schema-imports", "UTF-8")

      val payload2 =
        jsonContentOf(
          "/kg/resources/simple-resource.json",
          Map(quote("{priority}") -> "5", quote("{resourceId}") -> "10")
        )

      cl(Req(PUT, s"$kgBase/resources/$id1/${id2}/test-resource:10", headersJsonUser, payload2.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "fetch the payload wih metadata" in {
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1", headersJsonUser)).mapJson { (json, result) =>
        val expected = jsonContentOf(
          "/kg/resources/simple-resource-response.json",
          Map(
            quote("{priority}")  -> "5",
            quote("{rev}")       -> "1",
            quote("{resources}") -> s"$kgBase/resources/$id1",
            quote("{project}")   -> s"$adminBase/projects/$id1",
            quote("{iamBase}")   -> config.iam.uri.toString(),
            quote("{realm}")     -> config.iam.testRealm,
            quote("{user}")      -> config.iam.testUserSub
          )
        )
        result.status shouldEqual StatusCodes.OK
        json.removeFields("_createdAt", "_updatedAt") should equalIgnoreArrayOrder(expected)
      }
    }

    "fetch the original payload" in {
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1/source", headersJsonUser)).mapJson {
        (json, result) =>
          val expected =
            jsonContentOf(
              "/kg/resources/simple-resource.json",
              Map(quote("{priority}") -> "5", quote("{resourceId}") -> "1")
            )
          result.status shouldEqual StatusCodes.OK
          json should equalIgnoreArrayOrder(expected)
      }
    }
  }

  "cross-project resolvers" should {
    val resolverPayload =
      jsonContentOf(
        "/kg/resources/cross-project-resolver.json",
        Map(quote("{project}") -> id1, quote("{user}") -> config.iam.testUserSub)
      )
    "fail if the schema doesn't exist in the project" in {
      val payload = jsonContentOf(
        "/kg/resources/simple-resource.json",
        Map(quote("{priority}") -> "3", quote("{resourceId}") -> "1")
      )

      cl(Req(PUT, s"$kgBase/resources/$id2/test-schema/test-resource:1", headersJsonUser, payload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.NotFound)
    }

    "fail to create a cross-project-resolver for proj2 if identities are missing" in {

      cl(Req(POST, s"$kgBase/resolvers/$id2", headersJsonUser, resolverPayload.removeField("identities").toEntity))
        .mapResp(_.status shouldEqual StatusCodes.BadRequest)
    }

    "create a cross-project-resolver for proj2" in {

      cl(Req(POST, s"$kgBase/resolvers/$id2", headersJsonUser, resolverPayload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "update a cross-project-resolver for proj2" in {
      val updated = resolverPayload deepMerge Json.obj("priority" -> Json.fromInt(20))
      eventually {
        cl(Req(PUT, s"$kgBase/resolvers/$id2/example-id?rev=1", headersJsonUser, updated.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.OK)
      }
    }

    "fetch the update" in {
      val expected =
        jsonContentOf(
          "/kg/resources/cross-project-resolver-updated-resp.json",
          Map(
            quote("{project}")        -> id1,
            quote("{resources}")      -> s"$kgBase/resolvers/$id2",
            quote("{project-parent}") -> s"$adminBase/projects/$id2",
            quote("{iamBase}")        -> config.iam.uri.toString(),
            quote("{realm}")          -> config.iam.testRealm,
            quote("{user}")           -> config.iam.testUserSub
          )
        )
      cl(Req(GET, s"$kgBase/resolvers/$id2/example-id", headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json.removeField("_createdAt").removeField("_updatedAt") should equalIgnoreArrayOrder(expected)
      }
    }

    "wait for the cross-project resolver to be indexed" in {
      val expected = jsonContentOf(
        "/kg/resources/cross-project-resolver-list.json",
        Map(
          quote("{kgBase}")  -> s"$kgBase",
          quote("{projId}")  -> s"$id2",
          quote("{project}") -> s"$adminBase/projects/$id2",
          quote("{iamBase}") -> config.iam.uri.toString(),
          quote("{realm}")   -> config.iam.testRealm,
          quote("{user}")    -> config.iam.testUserSub
        )
      )
      eventually {
        cl(Req(GET, s"$kgBase/resolvers/$id2", headersJsonUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          removeSearchMetadata(json) shouldEqual expected
        }
      }
    }

    s"fetches a resource in project '${id1}' through project '${id2}' resolvers" in {

      cl(Req(GET, s"$kgBase/schemas/$id1/test-schema", headersJsonUser)).mapJson { (json, result1) =>
        result1.status shouldEqual StatusCodes.OK
        cl(Req(GET, s"$kgBase/resolvers/$id2/_/test-schema", headersJsonUser)).mapJson { (jsonResolved, result2) =>
          result2.status shouldEqual StatusCodes.OK
          jsonResolved should equalIgnoreArrayOrder(json)
        }
        cl(Req(GET, s"$kgBase/resolvers/$id2/example-id/test-schema", headersJsonUser)).mapJson {
          (jsonResolved, result2) =>
            result2.status shouldEqual StatusCodes.OK
            jsonResolved should equalIgnoreArrayOrder(json)
        }
        cl(Req(GET, s"$kgBase/resolvers/$id2/example-id/test-schema-2", headersJsonUser))
          .mapResp(_.status shouldEqual StatusCodes.NotFound)
        cl(Req(GET, s"$kgBase/resolvers/$id2/_/test-schema-2", headersJsonUser))
          .mapResp(_.status shouldEqual StatusCodes.NotFound)
      }
    }

    "resolve schema from the other project" in {
      val payload = jsonContentOf(
        "/kg/resources/simple-resource.json",
        Map(quote("{priority}") -> "3", quote("{resourceId}") -> "1")
      )

      eventually {
        cl(Req(PUT, s"$kgBase/resources/$id2/test-schema/test-resource:1", headersJsonUser, payload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }

  }

  "updating a resource" should {
    "send the update" in {
      val payload = jsonContentOf(
        "/kg/resources/simple-resource.json",
        Map(quote("{priority}") -> "3", quote("{resourceId}") -> "1")
      )

      cl(Req(PUT, s"$kgBase/resources/$id1/test-schema/test-resource:1?rev=1", headersJsonUser, payload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.OK)
    }
    "fetch the update" in {
      val expected = jsonContentOf(
        "/kg/resources/simple-resource-response.json",
        Map(
          quote("{priority}")  -> "3",
          quote("{rev}")       -> "2",
          quote("{resources}") -> s"$kgBase/resources/$id1",
          quote("{project}")   -> s"$adminBase/projects/$id1",
          quote("{iamBase}")   -> config.iam.uri.toString(),
          quote("{realm}")     -> config.iam.testRealm,
          quote("{user}")      -> config.iam.testUserSub
        )
      )
      forAll(List(s"$kgBase/resources/$id1/test-schema", s"$kgBase/resources/$id1/_")) { base =>
        cl(Req(GET, s"$base/test-resource:1", headersJsonUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeField("_createdAt").removeField("_updatedAt") shouldEqual expected
        }
      }
    }

    "fetch previous revision" in {
      val expected = jsonContentOf(
        "/kg/resources/simple-resource-response.json",
        Map(
          quote("{priority}")  -> "5",
          quote("{rev}")       -> "1",
          quote("{resources}") -> s"$kgBase/resources/$id1",
          quote("{project}")   -> s"$adminBase/projects/$id1",
          quote("{iamBase}")   -> config.iam.uri.toString(),
          quote("{realm}")     -> config.iam.testRealm,
          quote("{user}")      -> config.iam.testUserSub
        )
      )
      forAll(List(s"$kgBase/resources/$id1/test-schema", s"$kgBase/resources/$id1/_")) { base =>
        cl(Req(GET, s"$base/test-resource:1?rev=1", headersJsonUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeField("_createdAt").removeField("_updatedAt") should equalIgnoreArrayOrder(expected)
        }
      }
    }
  }

  "tagging a resource" should {

    "create a tag" in {
      val tag1 = jsonContentOf("/kg/resources/tag.json", Map(quote("{tag}") -> "v1.0.0", quote("{rev}") -> "1"))
      val tag2 = jsonContentOf("/kg/resources/tag.json", Map(quote("{tag}") -> "v1.0.1", quote("{rev}") -> "2"))

      cl(Req(POST, s"$kgBase/resources/$id1/test-schema/test-resource:1/tags?rev=2", headersJsonUser, tag1.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(POST, s"$kgBase/resources/$id1/_/test-resource:1/tags?rev=3", headersJsonUser, tag2.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "fetch a tagged value" in {

      val expectedTag1 = jsonContentOf(
        "/kg/resources/simple-resource-response.json",
        Map(
          quote("{priority}")  -> "3",
          quote("{rev}")       -> "2",
          quote("{resources}") -> s"$kgBase/resources/$id1",
          quote("{project}")   -> s"$adminBase/projects/$id1",
          quote("{iamBase}")   -> config.iam.uri.toString(),
          quote("{realm}")     -> config.iam.testRealm,
          quote("{user}")      -> config.iam.testUserSub
        )
      )
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1?tag=v1.0.1", headersJsonUser)).mapJson {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeField("_createdAt").removeField("_updatedAt") should equalIgnoreArrayOrder(expectedTag1)
      }

      val expectedTag2 = jsonContentOf(
        "/kg/resources/simple-resource-response.json",
        Map(
          quote("{priority}")  -> "5",
          quote("{rev}")       -> "1",
          quote("{resources}") -> s"$kgBase/resources/$id1",
          quote("{project}")   -> s"$adminBase/projects/$id1",
          quote("{iamBase}")   -> config.iam.uri.toString(),
          quote("{realm}")     -> config.iam.testRealm,
          quote("{user}")      -> config.iam.testUserSub
        )
      )
      cl(Req(GET, s"$kgBase/resources/$id1/_/test-resource:1?tag=v1.0.0", headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json.removeField("_createdAt").removeField("_updatedAt") should equalIgnoreArrayOrder(expectedTag2)
      }
    }
  }

  "listing resources" should {

    "list default resources" in {
      val mapping = Map(
        quote("{kgBase}")        -> kgBase.toString(),
        quote("{project-label}") -> id1,
        quote("{project}")       -> s"$adminBase/projects/$id1",
        quote("{iamBase}")       -> config.iam.uri.toString(),
        quote("{realm}")         -> config.iam.testRealm,
        quote("{user}")          -> config.iam.testUserSub
      )
      val resources = List(
        "resolvers" -> jsonContentOf("/kg/listings/default-resolver.json", mapping),
        "views"     -> jsonContentOf("/kg/listings/default-view.json", mapping),
        "storages"  -> jsonContentOf("/kg/listings/default-storage.json", mapping)
      )
      forAll(resources) {
        case (segment, expected) =>
          eventually {
            cl(Req(GET, s"$kgBase/$segment/$id1", headersJsonUser)).mapJson { (json, result) =>
              result.status shouldEqual StatusCodes.OK
              removeSearchMetadata(json) shouldEqual expected
            }
          }
      }
    }

    "add more resource to the project" in {

      forAll(2 to 5) { resourceId =>
        val payload = jsonContentOf(
          "/kg/resources/simple-resource.json",
          Map(quote("{priority}") -> "3", quote("{resourceId}") -> s"$resourceId")
        )
        cl(Req(PUT, s"$kgBase/resources/$id1/test-schema/test-resource:$resourceId", headersJsonUser, payload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }

    }

    "list the resources" in {
      val expected = jsonContentOf(
        "/kg/listings/response.json",
        Map(
          quote("{resources}") -> s"$kgBase/resources/$id1",
          quote("{project}")   -> s"$adminBase/projects/$id1",
          quote("{iamBase}")   -> config.iam.uri.toString(),
          quote("{realm}")     -> config.iam.testRealm,
          quote("{user}")      -> config.iam.testUserSub
        )
      )
      eventually {
        cl(Req(GET, s"$kgBase/resources/$id1/test-schema", headersJsonUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          removeSearchMetadata(json) shouldEqual expected
        }
      }
    }

    "return 400 when using both from and after" in {
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema?from=10&after=%5B%22test%22%5D", headersJsonUser)).mapJson {
        (json, result) =>
          result.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/kg/listings/from-and-after-error.json")
      }
    }

    "return 400 when from is bigger than limit" in {
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema?from=10001", headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.BadRequest
        json shouldEqual jsonContentOf("/kg/listings/from-over-limit-error.json")
      }
    }

    "list responses using after" in {

      val um = implicitly[FromEntityUnmarshaller[Json]]
      val result = Source
        .unfoldAsync(s"$kgBase/resources/$id1/test-schema?size=2") { searchUrl =>
          cl(Req(GET, searchUrl, headersJsonUser))
            .flatMap(res => um(res.entity))
            .map(
              json =>
                getNext(json).map { next =>
                  (next, getResults(json))
                }
            )
        }
        .mapConcat[Json](_.to[collection.immutable.Seq])
        .runWith(Sink.seq)
        .futureValue

      val expected = getResults(
        jsonContentOf(
          "/kg/listings/response.json",
          Map(
            quote("{resources}") -> s"$kgBase/resources/$id1",
            quote("{project}")   -> s"$adminBase/projects/$id1",
            quote("{iamBase}")   -> config.iam.uri.toString(),
            quote("{realm}")     -> config.iam.testRealm,
            quote("{user}")      -> config.iam.testUserSub
          )
        )
      )

      result shouldEqual expected
    }

    "create context" in {
      val payload = jsonContentOf("/kg/resources/simple-context.json")

      cl(Req(PUT, s"$kgBase/resources/$id1/_/test-resource:mycontext", headersJsonUser, payload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "create resource using the created context" in {
      val payload = jsonContentOf("/kg/resources/simple-resource-context.json")

      cl(Req(POST, s"$kgBase/resources/$id1/", headersJsonUser, payload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "update context" in {
      val payload = Json.obj("@context" -> Json.obj("alias" -> Json.fromString("http://example.com/alias")))

      cl(Req(PUT, s"$kgBase/resources/$id1/_/test-resource:mycontext?rev=1", headersJsonUser, payload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.OK)
    }

    "fetched previously created resource" in {
      val resourceId = URLEncoder.encode("http://example.com/base/myid", "UTF-8")
      cl(Req(GET, s"$kgBase/resources/$id1/_/${resourceId}", headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json.hcursor.get[String]("@id").right.value shouldEqual "http://example.com/base/myid"
        json.hcursor.get[String]("@type").right.value shouldEqual "http://example.com/type"
      }
    }

  }

  def getNext(json: Json): Option[String] =
    json.asObject.flatMap(_("_next")).flatMap(_.asString)

  def getResults(json: Json): Seq[Json] =
    removeSearchMetadata(json).asObject
      .flatMap(_("_results"))
      .flatMap(_.asArray)
      .getOrElse(Seq.empty)

  def removeSearchMetadata(json: Json): Json =
    json
      .removeField("_next")
      .hcursor
      .downField("_results")
      .withFocus(
        _.mapArray(
          _.map(
            _.removeFields("_createdAt", "_updatedAt")
          )
        )
      )
      .top
      .value
}
