package ch.epfl.bluebrain.nexus.perf.data.generation.types

import java.util.regex.Pattern

import akka.http.scaladsl.model.Uri
import ammonite.ops._
import io.circe.Json
import io.circe.parser._

import scala.collection.mutable

/**
  * Data type which holds the instance information
  */
sealed trait Data extends Product with Serializable {
  def path: BasePath
}

object Data extends DataNeighbours {

  /**
    * Failed to load instance information
    *
    * @param path the path in the filesystem of the instance
    */
  final case class FailedDataFormat(path: BasePath) extends Data

  /**
    * Failed to map the schema to a Uri
    *
    * @param path the path in the filesystem of the instance
    * @param schema the schema directory without an @id mapping
    */
  final case class FailedDataSchemaMap(path: BasePath, schema: String) extends Data

  /**
    * Successfully loaded instance information
    *
    * @param id            the unique identity of the instance
    * @param payload       the payload of the instance
    * @param path          the path in the filesystem of the instance
    * @param relationships the list of ids which have an outgoing relationship with this instance
    * @param org           the organization label
    * @param project       the project label
    * @param schema        the schema @id
    */
  final case class LocalData(
      id: String,
      payload: String,
      path: Path,
      relationships: List[String],
      org: String,
      project: String,
      schema: Uri
  ) extends Data {
    def relationEdges: List[(String, String)] = relationships.map(id -> _)

    def withReplacement(map: mutable.Map[String, String]): LocalData = {
      val replacements = relationships.map(v => v -> map.get(v)).collect {
        case (original, Some(replacement)) => original -> replacement
      }
      val p = replacements.foldLeft(payload) {
        case (acc, (original, replacement)) =>
          acc.replaceAll(Pattern.quote(original), replacement)
      }
      this.copy(payload = p, relationships = replace(relationships, map))
    }

  }

  private def replace(strings: List[String], map: mutable.Map[String, String]): List[String] =
    strings.map(string => map.getOrElse(string, string))

  object LocalData {

    /**
      * Construct [[Data]] from the provided ''path''.
      *
      * @param path the path in the filesystem of the instance
      */
    final def apply(path: Path)(implicit s: Settings): Data = {
      val payload = read(path)
      parse(payload).toOption
        .flatMap(
          json =>
            relationships(json).map {
              case (local, neighbors) =>
                val schemaName = (path / up).baseName
                s.schemasMap
                  .get(schemaName)
                  .map { schema =>
                    val proj = (path / up / up).baseName
                    val org  = (path / up / up / up).baseName
                    LocalData(local, json.noSpaces, path, neighbors, org, proj, schema)
                  }
                  .getOrElse(FailedDataSchemaMap(path, schemaName))
            }
        )
        .getOrElse(FailedDataFormat(path))
    }
  }
}

trait DataNeighbours {

  private val PREFIX = "local_"

  /**
    * Resolve the outgoing dependencies for the provided ''json''
    *
    * @param value the json
    * @return a [[Tuple2]] where the first part is the id of the instance and the second is the list of it's relationships.
    */
  def relationships(value: Json): Option[(String, List[String])] = {

    def inner(j: Json, keys: List[String]): List[String] =
      j.arrayOrObject[List[String]](
        keys,
        arr => arr.toList.flatMap(json => inner(json, keys)),
        obj =>
          obj.toList.flatMap {
            case ("@id", json) if json.isString =>
              json.as[String].toOption match {
                case Some(id) if id.startsWith(PREFIX) => id :: keys
                case _                                 => inner(json, keys)
              }
            case ("@context", _) => keys
            case (_, json)       => inner(json, keys)
          }
      )
    value.hcursor
      .get[String]("@id")
      .toOption
      .map(id => id -> inner(value, List.empty).filterNot(_ == id))
  }
}
