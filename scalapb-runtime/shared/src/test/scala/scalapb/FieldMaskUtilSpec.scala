package scalapb

import com.google.protobuf.field_mask.FieldMask
import utest._

object FieldMaskUtilSpec extends TestSuite {
  val tests = Tests {
    "FieldMaskUtil.toJsonString" - {
      // https://github.com/google/protobuf/blob/v3.6.0/java/util/src/test/java/com/google/protobuf/util/JsonFormatTest.java#L761-L770
      val x = FieldMask(Seq("foo.bar", "baz", "foo_bar.baz"))
      val expect = "foo.bar,baz,fooBar.baz"
      val json = FieldMaskUtil.toJsonString(x)
      assert(json == expect)
    }

    "FieldMaskExtensions.toSingleString should return string representation of " - {
      import FieldMaskUtil._

      "empty field mask" - {
        val emptyMask = FieldMask(Seq.empty)

        assert(
          emptyMask.toSingleString == ""
        )
      }

      "single path field mask" - {
        val singlePathMask = FieldMask(Seq("foo.bar.baz"))

        assert(
          singlePathMask.toSingleString == "foo.bar.baz"
        )
      }

      "multiple path field mask" - {
        val multiplePathMask = FieldMask(Seq("foo.bar", "baz", "a.b.c"))

        assert(
          multiplePathMask.toSingleString == "foo.bar,baz,a.b.c"
        )
      }
    }

    "FieldMaskExtensions.intersection" - {
      import FieldMaskUtil._
      assert(FieldMaskUtil.intersection(FieldMask(Seq("foo", "bar.baz", "bar.quz")), FieldMask(Seq("foo.bar", "bar"))).toSingleString == "foo.bar,bar.baz,bar.quz")

      assert(FieldMaskUtil.intersection(FieldMask(Seq("foo", "bar.baz", "bar.quz")), FieldMask(Seq())).toSingleString == "")

      assert(FieldMaskUtil.intersection(FieldMask(Seq("foo", "bar.baz", "bar.quz")), FieldMask(Seq("quz"))).toSingleString == "")

      assert(FieldMaskUtil.intersection(FieldMask(Seq("foo", "bar.baz", "bar.quz")), FieldMask(Seq("foo.bar"))).toSingleString == "foo.bar")

      assert(FieldMaskUtil.intersection(FieldMask(Seq("foo", "bar.baz", "bar.quz")), FieldMask(Seq("foo"))).toSingleString == "foo")

      assert(FieldMaskUtil.intersection(FieldMask(Seq("foo", "bar.baz", "bar.quz")), FieldMask(Seq("bar.foo"))).toSingleString == "")

      assert(FieldMaskUtil.intersection(FieldMask(Seq("foo", "bar.baz", "bar.quz")), FieldMask(Seq("bar"))).toSingleString == "bar.baz,bar.quz")
    }
  }
}
