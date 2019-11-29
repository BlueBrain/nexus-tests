package ch.epfl.bluebrain.nexus.perf

import java.net.URLEncoder
import java.util.UUID

import io.circe.parser.parse
import io.circe.{Json, JsonObject}
import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.http.Predef._

class UpdateSimulationNoSchema extends BaseSimulation {

  val project = config.update.project

  val revisions    = config.update.revisions
  val revisionStep = config.update.revisionsStep

  val revisionExpression: Expression[Boolean] = { session =>
    session("expectedRevisions").as[Int] >= session("currentRevision").as[Int]
  }

  val repeatCountExpression: Expression[Int] = { session =>
    Math.min(revisions / revisionStep, session("search_total").as[Int])
  }

  val scn = scenario("UpdateSimulationNoSchema")
    .feed(schemasFeeder)
    .exec { session =>
      val s = session("schema").as[String]
      session.set("encodedSchema", URLEncoder.encode(s, "UTF-8"))
    }
    .tryMax(config.http.retries) {
      exec(
        http("List Resources")
          .get(s"/resources/perftestorg/perftestproj$project/resource")
          check jsonPath("$.._total").ofType[Int].saveAs("search_total")
      ).repeat(repeatCountExpression, "instanceNumber")(
        exec { session =>
          val s              = session("schema").as[String]
          val instanceNumber = session("instanceNumber").as[Int] + 1
          session
            .set("encodedId", URLEncoder.encode(s"$s/ids/$instanceNumber", "UTF-8"))
            .set("expectedRevisions", Math.max((revisions - (instanceNumber - 1) * revisionStep) - 1, 1))
        }.exec(
            http("Get Resource By Id")
              .get(s"/resources/perftestorg/perftestproj$project/resource/$${encodedId}")
              .check(
                jsonPath("$.._rev")
                  .ofType[Int]
                  .saveAs("currentRevision"),
                bodyString.saveAs("savedPayload")
              )
          )
          .asLongAs(revisionExpression)(
            exec { session =>
              val json     = parse(session("savedPayload").as[String]).right.get
              val revision = json.asObject.getOrElse(JsonObject())("_rev").flatMap(_.asNumber).flatMap(_.toInt).get
              val update = json.mapObject { obj =>
                obj
                  .filterKeys(s => !s.startsWith("_"))
                  .add(s"nxv:updated${revision + 1}", Json.fromString(s"${UUID.randomUUID().toString}"))
              }
              session.set("updateRevision", revision).set("updatePayload", update.spaces2)
            }.exec(
                http("Update Resource")
                  .put(s"/resources/perftestorg/perftestproj$project/resource/$${encodedId}?rev=$${updateRevision}")
                  .body(StringBody("${updatePayload}"))
                  .header("Content-Type", "application/json")
              )
              .exec(
                http("Get Resource By Id")
                  .get(s"/resources/perftestorg/perftestproj$project/resource/$${encodedId}")
                  .check(
                    jsonPath("$.._rev")
                      .ofType[Int]
                      .saveAs("currentRevision"),
                    bodyString.saveAs("savedPayload")
                  )
              )
          )
      )
    }

  setUp(scn.inject(atOnceUsers(config.fetch.users)).protocols(httpConf))

}
