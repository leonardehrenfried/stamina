package stamina
package testkit

import org.scalatest._
import org.scalatest.events._
import org.scalatest.matchers.{BePropertyMatchResult, BePropertyMatcher}

import scala.reflect.ClassTag

class ScalatestTestGenerationSpec extends StaminaTestKitSpec {

  import TestDomain._

  case class ItemPersister(override val key: String) extends Persister[Item, V1](key)(None) {
    def persist(t: Item): Persisted = Persisted(key, currentVersion, ByteString())
    def unpersist(p: Persisted): Item = item1
  }

  "A spec generated by StaminaTestKit" should {
    val spec = new StaminaTestKit with WordSpecLike {
      val persisters = Persisters(ItemPersister("item1"))
      "TestDomainSerialization" should {
        persisters.generateTestsFor(
          sample(item1),
          sample("failing-item-2", item2)
        )
      }
    }

    "generate test for serialization roundtrips" in {
      spec.testNames should contain("TestDomainSerialization should persist and unpersist Item")
    }
    "generate test for deserialization of stored serialized items" in {
      spec.testNames should contain("TestDomainSerialization should deserialize the stored serialized form of Item version 1")
    }
    "executes test for successful serialization roundtrips" in {
      val myRep = execSpec(spec)
      val Some(res) = myRep.findResultEvent("TestDomainSerialization should persist and unpersist Item")
      res should be(anInstanceOf[TestSucceeded])

    }
    "executes test for unsuccessful serialization roundtrips" in {
      val myRep = execSpec(spec)
      val Some(res) = myRep.findResultEvent("TestDomainSerialization should persist and unpersist Item failing-item-2")
      res should be(anInstanceOf[TestFailed])
    }
    "executes test for successful for deserialization of stored serialized items" in {
      val myRep = execSpec(spec)
      val Some(res) = myRep.findResultEvent("TestDomainSerialization should deserialize the stored serialized form of Item version 1")
      res should be(anInstanceOf[TestSucceeded])
    }
    "executes test for unsuccessful for deserialization of stored serialized items" in {
      val myRep = execSpec(spec)
      val Some(res) = myRep.findResultEvent("TestDomainSerialization should deserialize the stored serialized form of Item failing-item-2 version 1")
      res should be(anInstanceOf[TestFailed])
    }

  }

  def anInstanceOf[T: ClassTag] = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    new BePropertyMatcher[AnyRef] {
      def apply(left: AnyRef) = BePropertyMatchResult(left.getClass.isAssignableFrom(clazz), s"an instance of ${clazz.getName}")
    }
  }

  private def execSpec(spec: WordSpecLike): EventRecordingReporter = {
    val myRep = new EventRecordingReporter
    spec.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
    myRep
  }

}
