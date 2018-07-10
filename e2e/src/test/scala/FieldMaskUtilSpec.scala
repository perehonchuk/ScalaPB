import org.scalatest._
import protobuf_unittest.unittest._
import com.google.protobuf.field_mask.FieldMask
import org.scalatest.matchers._
import scalapb.GeneratedMessage
import TestAllTypes.NestedMessage

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

  it should "be not valid" in {
    NestedTestAllTypes.isValid("nonexist") must be(false)
    NestedTestAllTypes.isValid("payload.nonexist") must be(false)
    NestedTestAllTypes.isValid(FieldMask(List("nonexist"))) must be(false)
    NestedTestAllTypes.isValid(FieldMask(List("payload", "nonexist"))) must be(false)
    NestedTestAllTypes.isValid("payload.repeated_nested_message.bb") must be(false)
    NestedTestAllTypes.isValid("payload.optional_int32.bb") must be(false)
  }

  "Merge with empty PMessage" should "be able to merge payload.optional_int32" in new emptyPMessageCtx {
    override def someFieldMask = FieldMask(Seq("payload.optional_int32"))
    override def expectedMessage = NestedTestAllTypes(
      payload = Some(
        TestAllTypes(
          optionalInt32 = Some(1234)
        )
      )
    )
  }

  it should "be able to merge payload.optional_nested_message" in new emptyPMessageCtx {
    override def someFieldMask = FieldMask(Seq("payload.optional_nested_message"))
    override def expectedMessage = NestedTestAllTypes(
      payload = Some(
        TestAllTypes(
          optionalNestedMessage = Some(NestedMessage(Some(5678)))
        )
      )
    )
  }

  it should "be able to merge payload.repeated_int32" in new emptyPMessageCtx {
    override def someFieldMask = FieldMask(Seq("payload.repeated_int32"))
    override def expectedMessage = NestedTestAllTypes(
      payload = Some(
        TestAllTypes(
          repeatedInt32 = Seq(4321)
        )
      )
    )
  }

  it should "be able to merge payload.repeated_nested_message" in new emptyPMessageCtx {
    override def someFieldMask = FieldMask(Seq("payload.repeated_nested_message"))
    override def expectedMessage = NestedTestAllTypes(
      payload = Some(
        TestAllTypes(
          repeatedNestedMessage = Seq(NestedMessage(Some(8765)))
        )
      )
    )
  }

  it should "be able to merge child.payload.optional_int32" in new emptyPMessageCtx {
    override def someFieldMask = FieldMask(Seq("child.payload.optional_int32"))
    override def expectedMessage = NestedTestAllTypes(
      child = Some(NestedTestAllTypes(
        payload = Some(TestAllTypes(
          optionalInt32 = Some(1234)
        ))
      ))
    )
  }

  it should "be able to merge child.payload.optional_nested_message" in new emptyPMessageCtx {
    override def someFieldMask = FieldMask(Seq("child.payload.optional_nested_message"))
    override def expectedMessage = NestedTestAllTypes(
      child = Some(NestedTestAllTypes(
        payload = Some(TestAllTypes(
          optionalNestedMessage = Some(NestedMessage(Some(5678)))
        ))
      ))
    )
  }

  it should "be able to merge child.payload.repeated_int32" in new emptyPMessageCtx {
    override def someFieldMask = FieldMask(Seq("child.payload.repeated_int32"))
    override def expectedMessage = NestedTestAllTypes(
      child = Some(NestedTestAllTypes(
        payload = Some(TestAllTypes(
          repeatedInt32 = Seq(4321)
        ))
      ))
    )
  }

  it should "be able to merge child.payload.repeated_nested_message" in new emptyPMessageCtx {
    override def someFieldMask = FieldMask(Seq("child.payload.repeated_nested_message"))
    override def expectedMessage = NestedTestAllTypes(
      child = Some(NestedTestAllTypes(
        payload = Some(TestAllTypes(
          repeatedNestedMessage = Seq(NestedMessage(Some(8765)))
        ))
      ))
    )
  }

  it should "be able to merge all fields" in new emptyPMessageCtx {
    override def someFieldMask = FieldMask(Seq("child", "payload"))
    override def expectedMessage = source
  }

  trait emptyPMessageCtx extends baseCtx {
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


    def someFieldMask: FieldMask
    def expectedMessage: GeneratedMessage
    val newMessage = NestedTestAllTypes.messageReads.read(scalapb.FieldMaskUtil.merge(someFieldMask, source.toPMessage))

    newMessage must beEqualToMessage(expectedMessage)
  }

  trait baseCtx {
    def beEqualToMessage(right: GeneratedMessage) = new Matcher[GeneratedMessage] {
      def apply(left: GeneratedMessage) = org.scalatest.matchers.MatchResult(
        left.companion.messageReads.read(left.toPMessage) == right,
        left + " was not equal to " + right,
        left + " was equal to " + right
      )
    }
  }
}