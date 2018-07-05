import org.scalatest._
import protobuf_unittest.unittest._
import com.google.protobuf.field_mask.FieldMask

class FieldMaskUtilSpec extends FlatSpec with MustMatchers {
  "NestedTestAllTypes" should "be valid" in {
    NestedTestAllTypes.isValid("payload") must be(true)
    NestedTestAllTypes.isValid("payload.optional_int32") must be(true)
    NestedTestAllTypes.isValid("payload.repeated_int32") must be(true)
    NestedTestAllTypes.isValid("payload.optional_nested_message") must be(true)
    NestedTestAllTypes.isValid("payload.repeated_nested_message") must be(true)
    NestedTestAllTypes.isValid(FieldMask(List("payload"))) must be(true)
    NestedTestAllTypes.isValid(FieldMask(List("payload.optional_nested_message.bb"))) must be(true)
  }

  "NestedTestAllTypes" should "be not valid" in {
    NestedTestAllTypes.isValid("nonexist") must be(false)
    NestedTestAllTypes.isValid("payload.nonexist") must be(false)
    NestedTestAllTypes.isValid(FieldMask(List("nonexist"))) must be(false)
    NestedTestAllTypes.isValid(FieldMask(List("payload", "nonexist"))) must be(false)
    NestedTestAllTypes.isValid("payload.repeated_nested_message.bb") must be(false)
    NestedTestAllTypes.isValid("payload.optional_int32.bb") must be(false)
  }
}