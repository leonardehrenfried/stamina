package stamina

import scala.reflect.ClassTag
import migrations._
import play.api.data.validation.ValidationError
import play.api.libs.json._
import scala.language.existentials

/**
 * An implementation of the stamina Persister that will use play-json
 * to read/write serialized values. It supports a DSL for specifying
 * migrations to apply to older versions of persisted values to bring
 * them up to date with the latest version.
 *
 * The DSL allows for any migration function that can transform an
 * instance of JsValue to another, migrated instance of JsValue but
 * by far the best way to implement these migration functions is to
 * use the play json transformations:
 *
 * @see https://www.playframework.com/documentation/2.5.x/ScalaJsonTransformers
 *
 */
package object playjson {
  /** Simple type alias for Migration[JsValue] */
  type JsonMigration = Migration[JsValue]

  /** Simple type alias for Migrator[JsValue, V] */
  type JsonMigrator[V <: Version] = Migrator[JsValue, V]

  /**
   * Creates a JsonMigrator[V1] that can function as a builder for
   * creating JsonMigrator[V2], etc. Its migration will be the identity
   * function so calling its migrate function will not have any effect.
   */
  def from[V <: V1: VersionInfo]: JsonMigrator[V] = migrations.from[JsValue, V]

  /**
   * Creates a JsonPersister[T, V1], i.e. a JsonPersister that will only persist
   * and unpersist version 1. Use this function to produce the initial persister
   * for a new domain class/event/entity.
   */
  def persister[T: ClassTag](key: String)(implicit format: Format[T]): JsonPersister[T, V1] = new V1JsonPersister[T](key)

  /**
   * Creates a JsonPersister[T, V] where V is a version greater than V1.
   * It will always persist instances of T to version V but it will use the specified
   * JsonMigrator[V] to migrate any values older than version V to version V before
   * unpersisting them.
   */
  def persister[T: ClassTag, V <: Version: VersionInfo: MigratableVersion](key: String, migrator: JsonMigrator[V])(implicit format: Format[T]): JsonPersister[T, V] = new VnJsonPersister[T, V](key, migrator)

  private[playjson] def toJsonBytes[T](t: T)(implicit writer: Writes[T]): ByteString = ByteString(Json.stringify(writer.writes(t)))
  private[playjson] def fromJsonBytes[T: ClassTag](bytes: ByteString)(implicit reader: Reads[T]): T = {
    val json = parseJson(bytes)
    def clazz = implicitly[reflect.ClassTag[T]].runtimeClass
    reader.reads(json).fold(errors ⇒ throw JsonValidationError(json, clazz, errors), success ⇒ success)
  }
  private[playjson] def parseJson(bytes: ByteString): JsValue = Json.parse(bytes.toArray)
}

package playjson {
  /**
   * Simple abstract marker superclass to unify (and hide) the two internal Persister implementations.
   */
  sealed abstract class JsonPersister[T: ClassTag, V <: Version: VersionInfo](key: String) extends Persister[T, V](key) {
    private[playjson] def cannotUnpersist(p: Persisted) =
      s"""JsonPersister[${implicitly[ClassTag[T]].runtimeClass.getSimpleName}, V$currentVersion](key = "$key") cannot unpersist data with key "${p.key}" and version ${p.version}."""
  }

  private[playjson] class V1JsonPersister[T: ClassTag](key: String)(implicit format: Format[T]) extends JsonPersister[T, V1](key) {
    def persist(t: T): Persisted = Persisted(key, currentVersion, toJsonBytes(t))
    def unpersist(p: Persisted): T = {
      if (canUnpersist(p)) fromJsonBytes[T](p.bytes)
      else throw new IllegalArgumentException(cannotUnpersist(p))
    }
  }

  private[playjson] class VnJsonPersister[T: ClassTag, V <: Version: VersionInfo: MigratableVersion](key: String, migrator: JsonMigrator[V])(implicit format: Format[T]) extends JsonPersister[T, V](key) {
    override def canUnpersist(p: Persisted): Boolean = p.key == key && migrator.canMigrate(p.version)

    def persist(t: T): Persisted = Persisted(key, currentVersion, toJsonBytes(t))
    def unpersist(p: Persisted): T = {
      if (canUnpersist(p)) {
        val preMigrationJson = parseJson(p.bytes)
        val postMigrationJson = migrator.migrate(preMigrationJson, p.version)
        postMigrationJson.validate[T].fold({ errors ⇒
          def clazz = implicitly[reflect.ClassTag[T]].runtimeClass
          throw JsonValidationError(postMigrationJson, clazz, errors)
        }, success ⇒ success)
      } else throw new IllegalArgumentException(cannotUnpersist(p))
    }
  }
}

case class JsonValidationError(jsValue: JsValue, `class`: Class[_], errors: Seq[(JsPath, Seq[ValidationError])]) extends RuntimeException {
  override def getMessage: String = s"Validation error in JSON ${Json.stringify(jsValue)}. Cannot convert to type ${`class`.getCanonicalName}. Errors: ${errors.toString}"
}
