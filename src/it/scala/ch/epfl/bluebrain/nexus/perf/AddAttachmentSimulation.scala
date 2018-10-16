package ch.epfl.bluebrain.nexus.perf
import java.net.URLEncoder

import ammonite.ops._
import io.gatling.core.Predef._
import io.gatling.http.Predef._

import scala.util.Random

class AddAttachmentSimulation extends BaseSimulation {

  val project = config.attachmentsConfig.project

  val instancesToAttachTo = config.attachmentsConfig.instances
  val attachments         = config.attachmentsConfig.attachmentsPerInstance
  val attachmentSize      = config.attachmentsConfig.attachmentSize

  val attachmentFile = pwd / 'tmp / "test_attachment"

  rm ! attachmentFile
  write(attachmentFile, Random.alphanumeric.take(attachmentSize).mkString)

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
