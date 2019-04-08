package ch.epfl.bluebrain.nexus.tests.kg

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.{StatusCodes, HttpRequest => Req}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import ch.epfl.bluebrain.nexus.tests.iam.types.AclListing
import io.circe.Json
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, Inspectors}

class ResourcesSpec extends BaseSpec with Eventually with Inspectors with CancelAfterFailure {

  val orgId   = genId()
  val projId1 = genId()
  val projId2 = genId()
  val id1     = s"$orgId/$projId1"
  val id2     = s"$orgId/$projId2"

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
      cl(Req(PUT, s"$adminBase/projects/$id1", headersUserAcceptJson, kgProjectReqEntity(name = id1)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PUT, s"$adminBase/projects/$id2", headersUserAcceptJson, kgProjectReqEntity(name = id2)))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }
  }

  "adding schema" should {
    "create a schema" in {
      val schemaPayload = jsonContentOf("/kg/schemas/simple-schema.json")

      eventually {
        cl(Req(PUT, s"$kgBase/schemas/$id1/test-schema", headersUserAcceptJson, schemaPayload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }
  }

  "creating a resource" should {
    "succeed if the payload is correct" in {
      val payload =
        jsonContentOf("/kg/resources/simple-resource.json",
                      Map(quote("{priority}") -> "5", quote("{resourceId}") -> "1"))

      eventually {
        cl(Req(PUT, s"$kgBase/resources/$id1/test-schema/test-resource:1", headersUserAcceptJson, payload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }

    "fetch the payload" in {
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1", headersUserAcceptJson)).mapJson {
        (json, result) =>
          val expected = jsonContentOf(
            "/kg/resources/simple-resource-response.json",
            Map(
              quote("{priority}")  -> "5",
              quote("{rev}")       -> "1",
              quote("{resources}") -> s"$kgBase/resources/$id1",
              quote("{project}")   -> s"$adminBase/projects/$id1",
              quote("{iamBase}")   -> config.iam.uri.toString(),
              quote("{realm}")     -> config.iam.testRealm,
              quote("{user}")      -> config.iam.userSub
            )
          )
          result.status shouldEqual StatusCodes.OK
          json.removeFields("_createdAt", "_updatedAt") should equalIgnoreArrayOrder(expected)
      }
    }
  }

  "cross-project resolvers" should {
    val resolverPayload =
      jsonContentOf("/kg/resources/cross-project-resolver.json",
                    Map(quote("{project}") -> id1, quote("{user}") -> config.iam.userSub))
    "fail if the schema doesn't exist in the project" in {
      val payload = jsonContentOf("/kg/resources/simple-resource.json",
                                  Map(quote("{priority}") -> "3", quote("{resourceId}") -> "1"))

      cl(Req(PUT, s"$kgBase/resources/$id2/test-schema/test-resource:1", headersUserAcceptJson, payload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.NotFound)
    }

    "fail to create a cross-project-resolver for proj2 if identities are missing" in {

      cl(Req(POST, s"$kgBase/resolvers/$id2", headersUserAcceptJson, resolverPayload.removeField("identities").toEntity))
        .mapResp(_.status shouldEqual StatusCodes.BadRequest)
    }

    "create a cross-project-resolver for proj2" in {

      cl(Req(POST, s"$kgBase/resolvers/$id2", headersUserAcceptJson, resolverPayload.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "update a cross-project-resolver for proj2" in {
      val updated = resolverPayload deepMerge Json.obj("priority" -> Json.fromInt(20))
      eventually {
        cl(Req(PUT, s"$kgBase/resolvers/$id2/example-id?rev=1", headersUserAcceptJson, updated.toEntity))
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
            quote("{user}")           -> config.iam.userSub
          )
        )
      cl(Req(GET, s"$kgBase/resolvers/$id2/example-id", headersUserAcceptJson)).mapJson { (json, result) =>
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
          quote("{user}")    -> config.iam.userSub
        )
      )
      eventually {
        cl(Req(GET, s"$kgBase/resolvers/$id2", headersUserAcceptJson)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          removeSearchMetadata(json) shouldEqual expected
        }
      }
    }

    "resolve schema from the other project" in {
      val payload = jsonContentOf("/kg/resources/simple-resource.json",
                                  Map(quote("{priority}") -> "3", quote("{resourceId}") -> "1"))

      eventually {
        cl(Req(PUT, s"$kgBase/resources/$id2/test-schema/test-resource:1", headersUserAcceptJson, payload.toEntity))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }
    }

  }

  "updating a resource" should {
    "send the update" in {
      val payload = jsonContentOf("/kg/resources/simple-resource.json",
                                  Map(quote("{priority}") -> "3", quote("{resourceId}") -> "1"))

      cl(Req(PUT, s"$kgBase/resources/$id1/test-schema/test-resource:1?rev=1", headersUserAcceptJson, payload.toEntity))
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
          quote("{user}")      -> config.iam.userSub
        )
      )
      forAll(List(s"$kgBase/resources/$id1/test-schema", s"$kgBase/resources/$id1/_")) { base =>
        cl(Req(GET, s"$base/test-resource:1", headersUserAcceptJson)).mapJson { (json, result) =>
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
          quote("{user}")      -> config.iam.userSub
        )
      )
      forAll(List(s"$kgBase/resources/$id1/test-schema", s"$kgBase/resources/$id1/_")) { base =>
        cl(Req(GET, s"$base/test-resource:1?rev=1", headersUserAcceptJson)).mapJson { (json, result) =>
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

      cl(
        Req(POST,
            s"$kgBase/resources/$id1/test-schema/test-resource:1/tags?rev=2",
            headersUserAcceptJson,
            tag1.toEntity))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(POST, s"$kgBase/resources/$id1/_/test-resource:1/tags?rev=3", headersUserAcceptJson, tag2.toEntity))
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
          quote("{user}")      -> config.iam.userSub
        )
      )
      cl(Req(GET, s"$kgBase/resources/$id1/test-schema/test-resource:1?tag=v1.0.1", headersUserAcceptJson)).mapJson {
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
          quote("{user}")      -> config.iam.userSub
        )
      )
      cl(Req(GET, s"$kgBase/resources/$id1/_/test-resource:1?tag=v1.0.0", headersUserAcceptJson)).mapJson {
        (json, result) =>
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
        quote("{user}")          -> config.iam.userSub
      )
      val resources = List(
        "resolvers" -> jsonContentOf("/kg/listings/default-resolver.json", mapping),
        "views"     -> jsonContentOf("/kg/listings/default-view.json", mapping),
        "storages"  -> jsonContentOf("/kg/listings/default-storage.json", mapping)
      )
      forAll(resources) {
        case (segment, expected) =>
          eventually {
            cl(Req(GET, s"$kgBase/$segment/$id1", headersUserAcceptJson)).mapJson { (json, result) =>
              result.status shouldEqual StatusCodes.OK
              removeSearchMetadata(json) shouldEqual expected
            }
          }
      }
    }

    "add more resource to the project" in {

      forAll(2 to 5) { resourceId =>
        val payload = jsonContentOf("/kg/resources/simple-resource.json",
                                    Map(quote("{priority}") -> "3", quote("{resourceId}") -> s"$resourceId"))
        cl(
          Req(PUT,
              s"$kgBase/resources/$id1/test-schema/test-resource:$resourceId",
              headersUserAcceptJson,
              payload.toEntity))
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
          quote("{user}")      -> config.iam.userSub
        )
      )
      eventually {
        cl(Req(GET, s"$kgBase/resources/$id1/test-schema", headersUserAcceptJson)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          removeSearchMetadata(json) shouldEqual expected
        }
      }
    }
  }

  def removeSearchMetadata(json: Json): Json =
    json.hcursor
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
