package ch.epfl.bluebrain.nexus.perf

import java.net.URLEncoder
import java.util.concurrent.ThreadLocalRandom

import io.gatling.core.Predef._
import io.gatling.http.Predef._

class FetchSimulationNoSchema extends BaseSimulation {

  val project         = config.fetch.project
  val journeyDuration = config.fetch.duration

  val reads  = config.fetch.reads
  val writes = config.fetch.writes

  val scn = scenario("FetchSimulationNoSchema")
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
      ).during(journeyDuration)(
        repeat(reads)(
          exec { session =>
            val rnd = ThreadLocalRandom
              .current()
              .nextInt(session("search_total").as[Int]) + 1
            val s = session("schema").as[String]
            session.set("encodedId", URLEncoder.encode(s"$s/ids/$rnd", "UTF-8"))
          }.exec(
            http("Get Resource By Id")
              .get(s"/resources/perftestorg/perftestproj$project/$${encodedId}")
          )
        )
      )
    }

  setUp(scn.inject(atOnceUsers(config.fetch.users)).protocols(httpConf))

}
