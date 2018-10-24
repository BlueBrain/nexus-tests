package ch.epfl.bluebrain.nexus.perf
import java.net.URLEncoder
import java.util.UUID
import java.util.concurrent.ThreadLocalRandom

import io.circe.{Json, JsonObject}
import io.circe.parser.parse
import io.gatling.core.Predef._
import io.gatling.http.Predef._

class FetchMultipleProjectsSimulation extends BaseSimulation {

  private val numProjects = 5

  val journeyDuration = config.multipleProjectsConfig.fetchDuration

  val reads  = config.fetchConfig.reads
  val writes = config.fetchConfig.writes

  val scn = scenario("fetching data from multiple projects")
    .feed(schemasFeeder)
    .exec { session =>
      val s = session("schema").as[String]
      session.set("encodedSchema", URLEncoder.encode(s, "UTF-8"))
    }
    .during(journeyDuration)(
      tryMax(config.http.retries) {
        exec { session =>
          val project = ThreadLocalRandom
            .current()
            .nextInt(numProjects) + 1
          session.set("project", project)
        }.exec(http("list ${schema}")
            .get("/resources/perftestorg/perftestproj${project}/${encodedSchema}")
            check jsonPath("$.._total").ofType[Int].saveAs("search_total"))
          .repeat(reads)(
            exec { session =>
              val rnd = ThreadLocalRandom
                .current()
                .nextInt(session("search_total").as[Int]) + 1
              val s = session("schema").as[String]
              session.set("encodedId", URLEncoder.encode(s"$s/ids/$rnd", "UTF-8"))
            }.exec(
              http("fetch from ${schema}")
                .get("/resources/perftestorg/perftestproj${project}/${encodedSchema}/${encodedId}")
            )
          )
          .repeat(writes)(
            exec { session =>
              val rnd = ThreadLocalRandom
                .current()
                .nextInt(session("search_total").as[Int]) + 1
              val s = session("schema").as[String]
              session.set("encodedId", URLEncoder.encode(s"$s/ids/$rnd", "UTF-8"))

            }.exec(
                http("fetch from ${schema}")
                  .get("/resources/perftestorg/perftestproj${project}/${encodedSchema}/${encodedId}")
                  .check(bodyString.saveAs("savedPayload"))
              )
              .exec {
                session =>
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
                    "/resources/perftestorg/perftestproj${project}/$${encodedSchema}/${encodedId}?rev=${updateRevision}")
                  .body(StringBody("${updatePayload}"))
                  .header("Content-Type", "application/json")
              )
          )
      }
    )

  setUp(scn.inject(atOnceUsers(config.multipleProjectsConfig.parallelUsers)).protocols(httpConf))

}
