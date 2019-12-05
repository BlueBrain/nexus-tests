package ch.epfl.bluebrain.nexus.tests.kg.types

import java.time.Instant

import com.github.ghik.silencer.silent
import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._

final case class ViewStatistics(
    delayInSeconds: Long,
    discardedEvents: Long,
    failedEvents: Long,
    evaluatedEvents: Long,
    lastEventDateTime: Instant,
    lastProcessedEventDateTime: Instant,
    processedEvents: Long,
    remainingEvents: Long,
    totalEvents: Long
)

object ViewStatistics {

  @silent
  implicit val viewStatisticsDecoder: Decoder[ViewStatistics] = {
    implicit val config: Configuration = Configuration.default
    deriveConfiguredDecoder[ViewStatistics]
  }

}
