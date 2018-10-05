package ch.epfl.bluebrain.nexus.perf.data.generation

import ammonite.ops._
import ch.epfl.bluebrain.nexus.perf.data.generation.types.Data.{FailedDataFormat, FailedDataSchemaMap, LocalData}
import ch.epfl.bluebrain.nexus.perf.data.generation.types.Settings

final case class Templates(value: Seq[LocalData])

object Templates {

  /**
    * Loads templates from the folder 'src/main/resources/bbp' in the classpath.
    *
    * @param settings implicitly available [[Settings]]
    */
  final def apply(implicit settings: Settings): Templates = {
    val templates = (ls.rec ! pwd / "src" / "main" / "resources" / "bbp")
      .filter(_.isFile)
      .map { path =>
        LocalData(path) match {
          case data: LocalData           => data
          case data: FailedDataFormat    => throw WrongFormat(data.path)
          case data: FailedDataSchemaMap => throw SchemaNotMapped(data.path, data.schema)
        }
      }
    new Templates(templates)
  }

  @SuppressWarnings(Array("IncorrectlyNamedExceptions"))
  sealed abstract class TemplateLoadingError(message: String) extends Exception(message)

  @SuppressWarnings(Array("IncorrectlyNamedExceptions"))
  final case class SchemaNotMapped(resource: BasePath, schemas: String)
      extends TemplateLoadingError(s"Need to map the schema: '$schemas' in path '$resource'")

  @SuppressWarnings(Array("IncorrectlyNamedExceptions"))
  final case class WrongFormat(resource: BasePath)
      extends TemplateLoadingError(s"Resource in path '$resource' is not in JSON format")

}
