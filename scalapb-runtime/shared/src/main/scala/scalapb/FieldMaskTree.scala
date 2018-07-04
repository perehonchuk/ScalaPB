package scalapb

import com.google.protobuf.field_mask.FieldMask
import FieldMaskUtil._
import scala.collection.mutable

protected case class Node(children: mutable.TreeMap[String, Node] = mutable.TreeMap.empty)

case class FieldMaskTree(root: Node = Node(mutable.TreeMap.empty[String, Node])) extends Traversable[String] {
  private val FIELD_PATH_SEPARATOR_REGEX = "\\."

  def this(fieldMask: FieldMask) =
    this(FieldMaskTree.mergeFromFieldMask(FieldMaskTree(), fieldMask).root)

  def toFieldMask: FieldMask = FieldMask(getFieldPaths(root, "", List.empty))

  def mergeFromFieldMask(fieldMask: FieldMask): FieldMaskTree = {
    FieldMaskTree.mergeFromFieldMask(this, fieldMask)
  }

  def intersect(anotherTree: FieldMaskTree): FieldMaskTree = ???

  def addFieldPath(path: String): FieldMaskTree = {
    val pathParts = path.split(FIELD_PATH_SEPARATOR_REGEX).toList
    val newTree = this

    def innerAddFieldPath(node: Node, pathParts: List[String]): Unit = pathParts match {
      case Nil => node.children.clear()
      case x :: xs => node.children.get(x) match {
        case Some(n) => innerAddFieldPath(n, xs)
        case None =>
          val nextNode = Node()
          node.children.put(x, nextNode)
          innerAddFieldPath(nextNode, xs)
      }
    }

    innerAddFieldPath(newTree.root, pathParts)
    newTree
  }

  private def getFieldPaths(node: Node, path: String, paths: List[String]): List[String] = node.children match {
    case tree if tree.isEmpty => path :: paths
    case tree => tree.map { case (name, value) =>
      getFieldPaths(
        value,
        if (path.isEmpty) name else s"$path.$name",
        paths
      )
    }.reduce(_ ::: _)
  }

  override def foreach[U](f: String => U): Unit = ???

  override def toString(): String = toFieldMask.toSingleString
}

object FieldMaskTree {

  def mergeFromFieldMask(fieldMaskTree: FieldMaskTree, fieldMask: FieldMask): FieldMaskTree = {
    fieldMask.paths.foldLeft(fieldMaskTree)((acc, b) => acc.addFieldPath(b))
  }

}
