package ch.epfl.bluebrain.nexus.perf
import ch.epfl.bluebrain.nexus.perf.config.Settings
import com.typesafe.config.ConfigFactory
import io.gatling.core.Predef._
import io.gatling.http.Predef._

class BaseSimulation extends Simulation {

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
  )

  val schemasFeeder = schemas.toArray.circular
  val httpConf = http
    .baseUrl(config.kg.base.toString) // Here is the root for all relative URLs
    .authorizationHeader(s"Bearer ${config.http.token}")
}
