package ch.epfl.bluebrain.nexus.perf

import java.net.URLEncoder
import java.util.UUID
import java.util.concurrent.ThreadLocalRandom

import io.circe.parser.parse
import io.circe.{Json, JsonObject}
import io.gatling.core.Predef._
import io.gatling.http.Predef._

class FetchSimulation extends BaseSimulation {

  val project         = config.fetchConfig.project
  val journeyDuration = config.fetchConfig.duration

  val reads  = config.fetchConfig.reads
  val writes = config.fetchConfig.writes

  val scn = scenario("FetchSimulation")
    .feed(schemasFeeder)
    .exec { session =>
      val s = session("schema").as[String]
      session.set("encodedSchema", URLEncoder.encode(s, "UTF-8"))
    }
    .tryMax(config.http.retries) {
      exec(
        http("List Resources")
          .get(s"/resources/perftestorg/perftestproj$project/$${encodedSchema}")
          check jsonPath("$.._total").ofType[Int].saveAs("search_total"))
        .during(journeyDuration)(repeat(reads)(
          exec { session =>
            val rnd = ThreadLocalRandom
              .current()
              .nextInt(session("search_total").as[Int]) + 1
            val s = session("schema").as[String]
            session.set("encodedId", URLEncoder.encode(s"$s/ids/$rnd", "UTF-8"))
          }.exec(
            http("Get Resource By Id")
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
              http("Get Resource By Id")
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
              http("Update Resource")
                .put(
                  s"/resources/perftestorg/perftestproj$project/$${encodedSchema}/$${encodedId}?rev=$${updateRevision}")
                .body(StringBody("${updatePayload}"))
                .header("Content-Type", "application/json")
            )
        ))
    }

  setUp(scn.inject(atOnceUsers(config.fetchConfig.users)).protocols(httpConf))

}
