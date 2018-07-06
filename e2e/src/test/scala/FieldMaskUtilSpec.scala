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

  "TestAllTypes" should "be able to merge" in {
    import TestAllTypes.NestedMessage

    val someFieldMask = FieldMask(Seq("payload.optional_int32"))
    val value = TestAllTypes(
      optionalInt32 = Some(1234),
      optionalNestedMessage = Some(NestedMessage(Some(5678))),
      repeatedInt32 = Seq(4321),
      repeatedNestedMessage = Seq(NestedMessage(Some(8765)))
    )
    val source = NestedTestAllTypes(
      payload = Some(value),
      child = Some(NestedTestAllTypes(
        payload = Some(value)
      ))
    )

    // Now we have a message source with the following structure:
    //   [root] -+- payload -+- optional_int32
    //           |           +- optional_nested_message
    //           |           +- repeated_int32
    //           |           +- repeated_nested_message
    //           |
    //           +- child --- payload -+- optional_int32
    //                                 +- optional_nested_message
    //                                 +- repeated_int32
    //                                 +- repeated_nested_message

//    val newMessage = scalapb.FieldMaskUtil.merge(someFieldMask, source, NestedTestAllTypes.defaultInstance)
    val newMessage = scalapb.FieldMaskUtil.merge(someFieldMask, source.toPMessage)

    val expected = NestedTestAllTypes(
      payload = Some(
        TestAllTypes(
          optionalInt32 = Some(1234)
        )
      )
    )

    expected.toPMessage must be(newMessage)
  }
}