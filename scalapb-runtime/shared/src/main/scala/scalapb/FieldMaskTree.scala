package scalapb

import com.google.protobuf.field_mask.FieldMask

import scala.collection.mutable

protected case class Node(children: mutable.TreeMap[String,Node])

case class FieldMaskTree(root: Node = Node(mutable.TreeMap.empty[String, Node])) extends Traversable[String] {

  def this(fieldMask: FieldMask) =
    this(FieldMaskTree.mergeFromFieldMask(FieldMaskTree(), fieldMask).root)

  def toFieldMask: FieldMask = ???

  def mergeFromFieldMask(fieldMask: FieldMask): FieldMaskTree ={
    FieldMaskTree.mergeFromFieldMask(this, fieldMask)
  }

  def intersect(anotherTree: FieldMaskTree): FieldMaskTree = ???

  def addFieldPath(path: String): FieldMaskTree  = ???

  override def foreach[U](f: String => U): Unit = ???

  override def toString(): String = ???
}

object FieldMaskTree {

  def mergeFromFieldMask(fieldMaskTree: FieldMaskTree, fieldMask: FieldMask): FieldMaskTree = {
    fieldMask.paths.foldLeft(fieldMaskTree)((acc, b) => acc.addFieldPath(b))
  }

}
