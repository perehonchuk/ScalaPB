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
        mutable.HashMap("foo" ->
          Node(mutable.HashMap("baz" ->
            Node(mutable.HashMap("bar" -> Node(mutable.HashMap.empty))))
          )
        )
      )
    )
    val multipleNodesTree = FieldMaskTree(
      Node(
        mutable.HashMap("foo" ->
          Node(mutable.HashMap("baz" ->
            Node(mutable.HashMap("bar" -> Node(mutable.HashMap.empty))))
          ),
          "a" ->
            Node(mutable.HashMap("b" ->
              Node(mutable.HashMap("c" -> Node(mutable.HashMap.empty))))
            )
        )
      )
    )

    val anotherMultipleNodesTree = FieldMaskTree(
      Node(
        mutable.HashMap("x" ->
          Node(mutable.HashMap("y" ->
            Node(mutable.HashMap("z" -> Node(mutable.HashMap.empty))))
          ),
          "a" ->
            Node(mutable.HashMap("b" ->
              Node(mutable.HashMap("c" -> Node(mutable.HashMap.empty))))
            ),
          "foo" ->
            Node(mutable.HashMap("baz" ->
              Node(mutable.HashMap("bar" -> Node(mutable.HashMap.empty))))
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
              mutable.HashMap("foo" ->
                Node(mutable.HashMap("baz" -> Node(mutable.HashMap.empty)))
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
              mutable.HashMap("foo" ->
                Node(mutable.HashMap("baz" ->
                  Node(mutable.HashMap("bar" -> Node(mutable.HashMap.empty))))
                ),
                "a" ->
                  Node(mutable.HashMap("b" ->
                    Node(mutable.HashMap("c" -> Node(mutable.HashMap.empty))))
                  ),
                "x" ->
                  Node(mutable.HashMap("y" ->
                    Node(mutable.HashMap("z" -> Node(mutable.HashMap.empty))))
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

    "merge should" - {
      // ?TODO How to test it?

    }

    "intersect should" - {
      "return empty intersection with empty tree" - {
        assert(
          emptyTree.intersect(singleNodeTree) == emptyTree
        )
      }

      "return intersection with tree" - {
        assert(
          multipleNodesTree.intersect(singleNodeTree) == singleNodeTree
        )
      }

      "return intersaction with multiple nodde tree" - {
        assert(
          multipleNodesTree.intersect(anotherMultipleNodesTree) == multipleNodesTree
        )
      }
    }

  }
}
