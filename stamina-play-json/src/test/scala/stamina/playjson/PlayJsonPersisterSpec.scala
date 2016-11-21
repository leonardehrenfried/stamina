package stamina
package playjson

import play.api.libs.json._
import play.api.libs.json.Reads._

class PlayJsonPersisterSpec extends StaminaJsonSpec {
  import PlayJsonTestDomain._
  import PlayJsonTestDomain.Formats._

  val v1CartCreatedPersister = persister[CartCreatedV1]("cart-created")

  val setPriceTo1000 = (__ \ 'cart \ 'items).json.update {
    __.read[JsArray].map {
      case JsArray(array) ⇒ JsArray(array.map {
        case o: JsObject ⇒ o + ("price" → JsNumber(1000L))
        case r           ⇒ throw new IllegalArgumentException(s"$r was not an object.")
      })
    }
  }

  val v2CartCreatedPersister = persister[CartCreatedV2, V2](
    "cart-created",
    from[V1].to[V2](_.transform(setPriceTo1000).get)
  )

  val setTimestamp = __.json.update(
    __.read[JsObject].map { o ⇒ o ++ Json.obj("timestamp" → (System.currentTimeMillis - 3600000L)) }
  )

  val v3CartCreatedPersister = persister[CartCreatedV3, V3](
    "cart-created",
    from[V1]
      .to[V2](_.transform(setPriceTo1000).get)
      .to[V3](_.transform(setTimestamp).get)
  )

  "V1 persisters produced by PlayJsonPersister" should {
    "correctly persist and unpersist domain events " in {
      import v1CartCreatedPersister._
      unpersist(persist(v1CartCreated)) should equal(v1CartCreated)
    }
  }

  "V2 persisters with migrators produced by PlayJsonPersister" should {
    "correctly persist and unpersist domain events " in {
      import v2CartCreatedPersister._
      unpersist(persist(v2CartCreated)) should equal(v2CartCreated)
    }

    "correctly migrate and unpersist V1 domain events" in {
      val v1Persisted = v1CartCreatedPersister.persist(v1CartCreated)
      val v2Unpersisted = v2CartCreatedPersister.unpersist(v1Persisted)

      v2Unpersisted.cart.items.map(_.price).toSet should equal(Set(1000))
    }
  }

  "V3 persisters with migrators produced by PlayJsonPersister" should {
    "correctly persist and unpersist domain events " in {
      import v3CartCreatedPersister._
      unpersist(persist(v3CartCreated)) should equal(v3CartCreated)
    }

    "correctly migrate and unpersist V1 domain events" in {
      val v1Persisted = v1CartCreatedPersister.persist(v1CartCreated)
      val v2Persisted = v2CartCreatedPersister.persist(v2CartCreated)

      val v1Unpersisted = v3CartCreatedPersister.unpersist(v1Persisted)
      val v2Unpersisted = v3CartCreatedPersister.unpersist(v2Persisted)

      v1Unpersisted.cart.items.map(_.price).toSet should equal(Set(1000))
      v2Unpersisted.timestamp should (be > 0L and be < System.currentTimeMillis)
    }
  }
}
