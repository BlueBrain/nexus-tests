package ch.epfl.bluebrain.nexus.perf

import ch.epfl.bluebrain.nexus.commons.test.Resources
import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class ElasticSearchSimulation extends BaseSimulation with Resources {

  val queries = jsonContentOf("/es-queries.json").hcursor
    .downField("queries")
    .values
    .get
    .map(json => Map("query" -> json.noSpaces))
    .toArray
    .random

  val project = config.attachmentsConfig.project

  val scn = scenario("ES search")
    .feed(queries)
    .during(config.fetchConfig.duration) {
      exec(
        http("ES search")
          .post(s"/views/perftestorg/perftestproj$project/nxv:defaultElasticIndex/_search")
          .body(StringBody("${query}"))
          .asJson
      )
    }

  setUp(
    scn
      .inject(
        rampConcurrentUsers(0) to config.esSearchConfig.parallelUsers during (1 minutes),
        constantConcurrentUsers(config.esSearchConfig.parallelUsers) during config.esSearchConfig.duration
      )
      .protocols(httpConf))
}
