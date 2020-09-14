package ch.epfl.bluebrain.nexus.tests

import ch.epfl.bluebrain.nexus.tests.config.TestsConfig
import io.circe.Json
import io.circe.optics.JsonPath.root
import org.scalatest.{Assertion, OptionValues}
import org.scalatest.matchers.should.Matchers

trait Optics extends Matchers with OptionValues

object Optics extends Optics {

  def removeKeys(keys: Set[String]): Json => Json =
    keys.map { root.at(_).set(None) }.reduce(_ andThen _)

  private val realmKeysToIgnore = Set("_createdAt", "_createdBy", "_updatedAt", "_updatedBy")
  val filterRealmKeys: Json => Json = removeKeys(realmKeysToIgnore)

  private val metadataKeys = Set("_uuid", "_createdAt", "_updatedAt", "_organizationUuid")
  val filterMetadataKeys: Json => Json = removeKeys(metadataKeys)

  val filterResultMetadata = root._results.arr.modify( _.map(filterMetadataKeys))

  object admin {
    val `@id` = root.`@id`.string
    val `@type` = root.`@type`.string
    val _uuid = root._uuid.string
    val _label = root._label.string
    val description = root.description.string
    val _rev = root._rev.long
    val _deprecated = root._deprecated.boolean

    val base = root.base.string
    val vocab = root.vocab.string
    val apiMappings = root.apiMappings.json

    def validate(json: Json,
                 tpe: String,
                 idPrefix: String,
                 id: String,
                 desc: String,
                 rev: Long,
                 label: String,
                 deprecated: Boolean = false)
                (implicit config: TestsConfig): Assertion = {
      `@id`.getOption(json).value shouldEqual s"${config.deltaUri.toString()}/$idPrefix/$id"
      `@type`.getOption(json).value shouldEqual tpe
      _uuid.getOption(json).value should fullyMatch regex """[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"""
      _label.getOption(json).value shouldEqual label
      description.getOption(json).value shouldEqual desc
      _rev.getOption(json).value shouldEqual rev
      _deprecated.getOption(json).value shouldEqual deprecated
    }

    def validateProject(response: Json, payload: Json): Assertion = {
      base.getOption(response) shouldEqual base.getOption(payload)
      vocab.getOption(response) shouldEqual vocab.getOption(payload)
      apiMappings.getOption(response) shouldEqual apiMappings.getOption(payload)
    }

  }

  object keycloak {
    val access_token = root.access_token.string
  }

  object error {
    val `@type` = root.`@type`.string
  }

}