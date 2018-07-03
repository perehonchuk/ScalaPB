package scalapb

import com.google.protobuf.field_mask.FieldMask
import utest._

object FieldMaskTreeSpec extends TestSuite {
  val tests = Tests {
    val someFieldMask = FieldMask(Seq(""))

    "addFieldPath should" - {
      "return same tree when path is empty" - {

      }

      "return tree with added path" - {

      }

      "return tree with simplified path" - {

      }
    }

    "mergeFromFieldMask should" - {

      "return empty tree when empty field mask provided" - {

      }

      "return tree from field mask" - {

      }
    }

    "toFieldMask should" - {

      "convert empty tree to empty field mask" - {

      }

      "convert tree to field mask" - {

      }
    }

    "intersect should" - {
      "return empty intersection with empty tree" - {

      }

      "return intersection with tree" - {

      }
    }

    "foreach should" - {

      "traverse empty tree" - {

      }

      "traverse tree" - {

      }
    }

    "toString should return string interpretation" - {

    }
  }
}
