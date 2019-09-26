package ch.epfl.bluebrain.nexus.perf.config

import AppConfig._
import akka.http.scaladsl.model.Uri

import scala.concurrent.duration.FiniteDuration

final case class AppConfig(http: HttpConfig,
                           kg: KgConfig,
                           create: CreateConfig,
                           fetch: FetchConfig,
                           update: UpdateConfig,
                           tag: TagConfig,
                           attachments: AttachmentsConfig,
                           esSearch: EsSearchConfig,
                           blazegraph: BlazegraphConfig,
                           multipleProjects: MultipleProjectsConfig,
                           fullSimulation: FullSimulationConfig)

object AppConfig {

  final case class HttpConfig(token: String, anonymous: Boolean, retries: Int)

  final case class KgConfig(base: Uri)

  final case class CreateConfig(project: Int, size: Int, parallelUsers: Int)

  final case class FetchConfig(project: Int, duration: FiniteDuration, reads: Int, writes: Int, users: Int)

  final case class UpdateConfig(project: Int, revisions: Int, revisionsStep: Int)

  final case class TagConfig(tags: Int)

  final case class AttachmentsConfig(project: Int,
                                     instances: Int,
                                     attachmentSize: Int,
                                     attachmentsPerInstance: Int,
                                     parallelUsers: Int,
                                     duration: FiniteDuration)

  final case class EsSearchConfig(project: Int, duration: FiniteDuration, parallelUsers: Int)
  final case class BlazegraphConfig(project: Int, duration: FiniteDuration, parallelUsers: Int)

  final case class MultipleProjectsConfig(projects: Int,
                                          instancesPerProject: Int,
                                          parallelUsers: Int,
                                          fetchDuration: FiniteDuration,
  )

  final case class FullSimulationConfig(project: Int,
                                        duration: FiniteDuration,
                                        users: Int,
                                        repeats: Int,
                                        fetchPercentage: Double,
                                        fetchAndUpdatePercentage: Double,
                                        fetchAndGetByRevisionPercentage: Double,
                                        blazegraphSearchPercentage: Double,
                                        esSearchPercentage: Double,
                                        maxResources: Int)
}
