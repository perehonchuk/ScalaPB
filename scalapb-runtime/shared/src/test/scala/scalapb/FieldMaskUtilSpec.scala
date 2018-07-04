package scalapb

import com.google.protobuf.field_mask.FieldMask
import utest._

object FieldMaskUtilSpec extends TestSuite {
  val tests = Tests {
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
  }
}
