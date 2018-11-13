package ch.epfl.bluebrain.nexus.perf

import ch.epfl.bluebrain.nexus.commons.test.Resources
import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class BlazegraphSimulation extends BaseSimulation with Resources {

  val queries = List(
    Map("query" -> contentOf("/person-by-type.sparql")),
    Map("query" -> contentOf("/subject-by-species.sparql")),
    Map("query" -> contentOf("/traces-by-species.sparql")),
    Map("query" -> contentOf("/traces-by-type.sparql"))
  ).toArray.random

  val project = config.blazegraphConfig.project

  val scn = scenario("BlazegraphSimulation")
    .feed(queries)
    .during(config.blazegraphConfig.duration) {
      exec(
        http("BlazeGraph Query")
          .post(s"/views/perftestorg/perftestproj$project/nxv:defaultSparqlIndex/sparql")
          .body(StringBody("${query}"))
          .header("Content-Type", "text/plain")
      )
    }

  setUp(
    scn
      .inject(
        rampConcurrentUsers(0) to config.blazegraphConfig.parallelUsers during (1 minutes),
        constantConcurrentUsers(config.blazegraphConfig.parallelUsers) during config.blazegraphConfig.duration
      )
      .protocols(httpConf))
}
