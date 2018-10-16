package ch.epfl.bluebrain.nexus.perf
import java.net.URLEncoder

import ammonite.ops._
import ch.epfl.bluebrain.nexus.perf.config.Settings
import com.typesafe.config.ConfigFactory
import io.gatling.core.Predef._
import io.gatling.http.Predef._

import scala.util.Random

class AddAttachmentSimulation extends Simulation {

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

  val project = config.attachmentsConfig.project

  val instancesToAttachTo = config.attachmentsConfig.instances
  val attachments         = config.attachmentsConfig.attachmentsPerInstance
  val attachemntSize      = config.attachmentsConfig.attachmentSize

  val attachmentFile = pwd / 'tmp / s"attachment_${Random.alphanumeric.take(10).mkString}"

  write(attachmentFile, Random.alphanumeric.take(attachemntSize).mkString)

  val scn = scenario("attach")
    .feed(schemasFeeder)
    .exec { session =>
      val s = session("schema").as[String]
      session.set("encodedSchema", URLEncoder.encode(s, "UTF-8"))
    }
    .tryMax(config.http.retries) {
      repeat(instancesToAttachTo, "instanceNumber")(
        exec { session =>
          val s              = session("schema").as[String]
          val instanceNumber = session("instanceNumber").as[Int] + 1
          session
            .set("encodedId", URLEncoder.encode(s"$s/ids/$instanceNumber", "UTF-8"))
        }.repeat(attachments, "attachmentsCounter")(
          exec { session =>
            session.set("attachment", session("attachmentsCounter").as[Int] + 1)
          }.exec(
              http("fetch from ${schema}")
                .get(s"/resources/perftestorg/perftestproj$project/$${encodedSchema}/$${encodedId}")
                .check(jsonPath("$.._rev")
                  .ofType[Int]
                  .saveAs("currentRevision"))
            )
            .exec(
              http("attach to ${schema}")
                .put(s"/resources/perftestorg/perftestproj$project/$${encodedSchema}/$${encodedId}/attachments/attachment$${attachment}?rev=$${currentRevision}")
                .formUpload("file", attachmentFile.toString())
            )
        )
      )
    }

  setUp(scn.inject(atOnceUsers(schemas.size)).protocols(httpConf))
}
