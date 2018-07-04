import org.scalatest._
import protobuf_unittest.unittest._

class FieldMaskUtilSpec extends FlatSpec with MustMatchers {
  "FieldMaskUtil" should "be valid" in {
    NestedTestAllTypes.isValid("payload") must be(true)
//    assertTrue(FieldMaskUtil.isValid(classOf[Nothing], "payload"))
//    assertFalse(FieldMaskUtil.isValid(classOf[Nothing], "nonexist"))
//    assertTrue(FieldMaskUtil.isValid(classOf[Nothing], "payload.optional_int32"))
//    assertTrue(FieldMaskUtil.isValid(classOf[Nothing], "payload.repeated_int32"))
//    assertTrue(FieldMaskUtil.isValid(classOf[Nothing], "payload.optional_nested_message"))
//    assertTrue(FieldMaskUtil.isValid(classOf[Nothing], "payload.repeated_nested_message"))
//    assertFalse(FieldMaskUtil.isValid(classOf[Nothing], "payload.nonexist"))
//
//    assertTrue(FieldMaskUtil.isValid(classOf[Nothing], FieldMaskUtil.fromString("payload")))
//    assertFalse(FieldMaskUtil.isValid(classOf[Nothing], FieldMaskUtil.fromString("nonexist")))
//    assertFalse(FieldMaskUtil.isValid(classOf[Nothing], FieldMaskUtil.fromString("payload,nonexist")))
//
//    assertTrue(FieldMaskUtil.isValid(NestedTestAllTypes.getDescriptor, "payload"))
//    assertFalse(FieldMaskUtil.isValid(NestedTestAllTypes.getDescriptor, "nonexist"))
//
//    assertTrue(FieldMaskUtil.isValid(NestedTestAllTypes.getDescriptor, FieldMaskUtil.fromString("payload")))
//    assertFalse(FieldMaskUtil.isValid(NestedTestAllTypes.getDescriptor, FieldMaskUtil.fromString("nonexist")))
//
//    assertTrue(FieldMaskUtil.isValid(classOf[Nothing], "payload.optional_nested_message.bb"))
//    // Repeated fields cannot have sub-paths.
//    assertFalse(FieldMaskUtil.isValid(classOf[Nothing], "payload.repeated_nested_message.bb"))
//    // Non-message fields cannot have sub-paths.
//    assertFalse(FieldMaskUtil.isValid(classOf[Nothing], "payload.optional_int32.bb"))
  }
}