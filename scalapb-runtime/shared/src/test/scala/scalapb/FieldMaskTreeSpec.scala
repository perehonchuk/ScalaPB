package scalapb

import com.google.protobuf.field_mask.FieldMask
import utest._

import scala.collection.mutable

object FieldMaskTreeSpec extends TestSuite {
  val tests = Tests {
    val someFieldMask = FieldMask(Seq(""))
    val emptyTree = FieldMaskTree()
    val singleNodeTree = FieldMaskTree(
      Node(
        mutable.TreeMap("foo" ->
          Node(mutable.TreeMap("baz" ->
            Node(mutable.TreeMap("bar" -> Node(mutable.TreeMap.empty))))
          )
        )
      )
    )
    val multipleNodesTree = FieldMaskTree(
      Node(
        mutable.TreeMap("foo" ->
          Node(mutable.TreeMap("baz" ->
            Node(mutable.TreeMap("bar" -> Node(mutable.TreeMap.empty))))
          ),
          "a" ->
            Node(mutable.TreeMap("b" ->
              Node(mutable.TreeMap("c" -> Node(mutable.TreeMap.empty))))
            )
        )
      )
    )

    "addFieldPath should" - {
      "return same tree when path is empty" - {
        assert(
          emptyTree.addFieldPath("") == emptyTree
        )
      }

      "return tree with single added path" - {
        assert(
          emptyTree.addFieldPath("foo.baz.bar") == singleNodeTree
        )
      }

      "return tree with multiple added paths" - {
        assert(
          singleNodeTree.addFieldPath("a.b.c") == multipleNodesTree

        )
      }

      "return tree with simplified path" - {
        assert(
          singleNodeTree.addFieldPath("foo.baz") == FieldMaskTree(
            Node(
              mutable.TreeMap("foo" ->
                Node(mutable.TreeMap("baz" -> Node(mutable.TreeMap.empty)))
              )
            )
          )
        )
      }
    }

    "mergeFromFieldMask should" - {

      "return the same tree when empty field mask provided" - {
        assert(
          emptyTree.mergeFromFieldMask(FieldMask(Seq.empty)) == emptyTree
        )
      }

      "return single node tree from field mask applied to empty tree" - {
        assert(
          emptyTree.mergeFromFieldMask(FieldMask(Seq("foo.baz.bar"))) == singleNodeTree
        )
      }

      "return multiple node tree from single path field mask applied to single node tree" - {
        assert(
          singleNodeTree.mergeFromFieldMask(FieldMask(Seq("a.b.c"))) == multipleNodesTree
        )
      }

      "return multiple node tree from multiple path field mask applied to single node tree" - {
        assert(
          singleNodeTree.mergeFromFieldMask(FieldMask(Seq("a.b.c", "x.y.z"))) == FieldMaskTree(
            Node(
              mutable.TreeMap("foo" ->
                Node(mutable.TreeMap("baz" ->
                  Node(mutable.TreeMap("bar" -> Node(mutable.TreeMap.empty))))
                ),
                "a" ->
                  Node(mutable.TreeMap("b" ->
                    Node(mutable.TreeMap("c" -> Node(mutable.TreeMap.empty))))
                  ),
                "x" ->
                  Node(mutable.TreeMap("y" ->
                    Node(mutable.TreeMap("z" -> Node(mutable.TreeMap.empty))))
                  )
              )
            )
          )
        )
      }
    }

    "toFieldMask should" - {

      "convert empty tree to empty field mask" - {
        emptyTree.toFieldMask == FieldMask(Seq.empty)
      }

      "convert single node tree to field mask" - {
        singleNodeTree.toFieldMask == FieldMask(Seq("foo.baz.bar"))
      }

      "convert multiple nodes tree to field mask" - {
        multipleNodesTree.toFieldMask == FieldMask(Seq("foo.baz.bar", "a.b.c"))
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
  }
}
