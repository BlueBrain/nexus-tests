package ch.epfl.bluebrain.nexus.perf

import java.net.URLEncoder
import java.util.UUID
import java.util.concurrent.ThreadLocalRandom

import ch.epfl.bluebrain.nexus.perf.config.Settings
import com.typesafe.config.ConfigFactory
import io.circe.{Json, JsonObject}
import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.circe.parser.parse

class FetchSimulation extends Simulation {

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

  val project         = config.fetchConfig.project
  val journeyDuration = config.fetchConfig.duration

  val reads  = config.fetchConfig.reads
  val writes = config.fetchConfig.writes

  val scn = scenario("fetching data")
    .feed(schemas)
    .exec { session =>
      val s = session("schema").as[String]
      session.set("encodedSchema", URLEncoder.encode(s, "UTF-8"))
    }
    .exec(http("list ${schema}")
      .get(s"/resources/perftestorg/perftestproj$project/$${encodedSchema}")
      check jsonPath("$.._total").ofType[Int].saveAs("search_total"))
    .during(journeyDuration)(
      repeat(reads)(
        exec { session =>
          val rnd = ThreadLocalRandom
            .current()
            .nextInt(session("search_total").as[Int]) + 1
          val s = session("schema").as[String]
          session.set("encodedId", URLEncoder.encode(s"$s/ids/$rnd", "UTF-8"))
        }.exec(
          http("fetch from ${schema}")
            .get(s"/resources/perftestorg/perftestproj$project/$${encodedSchema}/$${encodedId}")
        )
      ).repeat(writes)(
        exec { session =>
          val rnd = ThreadLocalRandom
            .current()
            .nextInt(session("search_total").as[Int]) + 1
          val s = session("schema").as[String]
          session.set("encodedId", URLEncoder.encode(s"$s/ids/$rnd", "UTF-8"))

        }.exec(
            http("fetch from ${schema}")
              .get(s"/resources/perftestorg/perftestproj$project/$${encodedSchema}/$${encodedId}")
              .check(bodyString.saveAs("savedPayload"))
          )
          .exec { session =>
            val json     = parse(session("savedPayload").as[String]).right.get
            val revision = json.asObject.getOrElse(JsonObject())("_rev").flatMap(_.asNumber).flatMap(_.toInt).get
            val update = json.mapObject { obj =>
              obj
                .filterKeys(s => !s.startsWith("_"))
                .add(s"nxv:updated${revision + 1}", Json.fromString(s"${UUID.randomUUID().toString}"))
            }
            session.set("updateRevision", revision).set("updatePayload", update.spaces2)
          }
          .exec(
            http("update ${schema}")
              .put(
                s"/resources/perftestorg/perftestproj$project/$${encodedSchema}/$${encodedId}?rev=$${updateRevision}")
              .body(StringBody("${updatePayload}"))
              .header("Content-Type", "application/json")
          )
      ))
  //  setUp(scn.inject(constantUsersPerSec(1) during (30 seconds)).protocols(httpConf))

  setUp(scn.inject(atOnceUsers(config.fetchConfig.users)).protocols(httpConf))

}
