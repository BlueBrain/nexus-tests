package ch.epfl.bluebrain.nexus.perf

import java.net.URLEncoder
import java.util.UUID

import ch.epfl.bluebrain.nexus.perf.config.Settings
import com.typesafe.config.ConfigFactory
import io.circe.{Json, JsonObject}
import io.circe.parser.parse
import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.http.Predef._

class UpdateSimulation extends Simulation {
  val config = new Settings(ConfigFactory.parseResources("perf-tests.conf").resolve()).appConfig

  val schemas = List(
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/experiment/wholecellpatchclamp"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/core/subject"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/core/slice"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/core/protocol"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/experiment/patchedcellslice"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/experiment/patchedcellcollection"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/experiment/patchedcell"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/experiment/brainslicing"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/tracegeneration"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/trace"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/stimulusexperiment"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/core/person")
  ).toArray.circular

  val httpConf = http
    .baseUrl(config.kg.base.toString) // Here is the root for all relative URLs
    .authorizationHeader(s"Bearer ${config.http.token}")

  val project = config.updateConfig.project

  val revisions    = config.updateConfig.revisions
  val revisionStep = config.updateConfig.revisionsStep

  val revisionExpression: Expression[Boolean] = { session =>
    session("expectedRevisions").as[Int] >= session("currentRevision").as[Int]
  }

  val repeatCountExpression: Expression[Int] = { session =>
    Math.min(revisions / revisionStep, session("search_total").as[Int])
  }

  val scn = scenario("update")
    .feed(schemas)
    .exec { session =>
      val s = session("schema").as[String]
      session.set("encodedSchema", URLEncoder.encode(s, "UTF-8"))
    }
    .tryMax(config.http.retries) {
      exec(
        http("list ${schema}")
          .get(s"/resources/perftestorg/perftestproj$project/$${encodedSchema}")
          check jsonPath("$.._total").ofType[Int].saveAs("search_total"))
        .repeat(repeatCountExpression, "instanceNumber")(
          exec { session =>
            val s              = session("schema").as[String]
            val instanceNumber = session("instanceNumber").as[Int] + 1
            session
              .set("encodedId", URLEncoder.encode(s"$s/ids/$instanceNumber", "UTF-8"))
              .set("expectedRevisions", Math.max((revisions - (instanceNumber - 1) * revisionStep) - 1, 1))
          }.exec(
              http("fetch from ${schema}")
                .get(s"/resources/perftestorg/perftestproj$project/$${encodedSchema}/$${encodedId}")
                .check(jsonPath("$.._rev")
                         .ofType[Int]
                         .saveAs("currentRevision"),
                       bodyString.saveAs("savedPayload"))
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
                  http("update ${schema}")
                    .put(s"/resources/perftestorg/perftestproj$project/$${encodedSchema}/$${encodedId}?rev=$${updateRevision}")
                    .body(StringBody("${updatePayload}"))
                    .header("Content-Type", "application/json")
                )
                .exec(
                  http("fetch from ${schema}")
                    .get(s"/resources/perftestorg/perftestproj$project/$${encodedSchema}/$${encodedId}")
                    .check(jsonPath("$.._rev")
                             .ofType[Int]
                             .saveAs("currentRevision"),
                           bodyString.saveAs("savedPayload"))
                )
            )
        )
    }

  setUp(scn.inject(atOnceUsers(12)).protocols(httpConf))

}
