package ch.epfl.bluebrain.nexus.tests.kg.types

import java.time.Instant

import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._

final case class ViewStatistics(delayInSeconds: Long,
                                discardedEvents: Long,
                                evaluatedEvents: Long,
                                lastEventDateTime: Instant,
                                lastProcessedEventDateTime: Instant,
                                processedEvents: Long,
                                remainingEvents: Long,
                                totalEvents: Long)

object ViewStatistics {
  private implicit val config: Configuration                  = Configuration.default
  implicit val viewStatisticsDecoder: Decoder[ViewStatistics] = deriveDecoder[ViewStatistics]

}
