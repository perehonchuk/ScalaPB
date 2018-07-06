package scalapb

import com.google.protobuf.field_mask.FieldMask
import FieldMaskUtil._
import scalapb.descriptors.{PEmpty, PMessage, PValue}

import scala.collection.mutable

protected case class Node(children: mutable.HashMap[String, Node] = mutable.HashMap.empty)

case class FieldMaskTree(root: Node = Node(mutable.HashMap.empty[String, Node])) {
  type Path = Seq[String]

  private val FIELD_PATH_SEPARATOR_REGEX = "\\."

  def toFieldMask: FieldMask = FieldMask(getFieldPaths(root, "", List.empty))

  def mergeFromFieldMask(fieldMask: FieldMask): FieldMaskTree = {
    FieldMaskTree.mergeFromFieldMask(this, fieldMask)
  }

  def intersect(anotherTree: FieldMaskTree): FieldMaskTree = {
    def intersectInternal(first: Node, second: Node): Node = {
      val intersect: collection.Set[String] = first.children.keySet.intersect(second.children.keySet)
      val tuples = intersect.map(k => k -> intersectInternal(first.children(k), second.children(k))).toSeq
      Node(mutable.HashMap(tuples: _*))
    }
    FieldMaskTree(intersectInternal(this.root, anotherTree.root))
  }

  def addFieldPath(path: String): FieldMaskTree = {
    val pathParts = path.split(FIELD_PATH_SEPARATOR_REGEX).toList
    val newTree = this

    def innerAddFieldPath(node: Node, parts: Path): Unit = parts match {
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

  def merge(mask: FieldMask, source: PMessage, destination: PMessage): PMessage = {
    val tree = FieldMaskTree(mask)
    val paths: List[Path] = getFieldPaths(tree.root).map(_.split(FIELD_PATH_SEPARATOR_REGEX).toList)

    paths
      .map(path => (path, getByPath(path, source)))
      .foldLeft(destination)((acc, elem) => setByPath(elem._1, elem._2, acc))
  }

  def merge(mask: FieldMask, source: PMessage): PMessage = {
    merge(mask, source, filterFields(mask, source))
  }

  private def filterFields(mask: FieldMask, source: PMessage): PMessage = {
    def innerMerge(path: Path, source: PMessage): PMessage = path match {
      case Nil => source
      case x :: xs =>
        source.copy(
          value = source.value.filterKeys(_.name == x).map {
            case (descriptor, value: PMessage) => (descriptor, innerMerge(xs, value))
            case another => another
          }
        )
    }

    val tree = FieldMaskTree(mask)
    val splittedPaths = getSplittedPaths(mask.paths)
    val firstLevelFiltered = source.value.map(p => (p._1.name, p)).filterKeys(tree.root.children.keySet)

    source.copy(
      value = firstLevelFiltered.values.map {
        case (descriptor, value: PMessage) => (
          descriptor,
          innerMerge(
            splittedPaths
              .find(_.head == descriptor.name)
              .getOrElse(Seq.empty[String]).tail,
            value
          )
        )
        case leaveValue => leaveValue
      }.toMap
    )
  }

  private def getFieldPaths(node: Node, path: String = "", paths: List[String] = List.empty): List[String] = node.children match {
    case tree if tree.isEmpty => path :: paths
    case tree => tree.map { case (name, value) =>
      getFieldPaths(
        value,
        if (path.isEmpty) name else s"$path.$name",
        paths
      )
    }.reduce(_ ::: _)
  }

  private def getSplittedPaths(unsplittedPaths: Seq[String]): Seq[Path] =
    unsplittedPaths.map(_.split(FIELD_PATH_SEPARATOR_REGEX).toList)

  private def getByPath(path: Path, source: PMessage): PValue = path match {
    case Nil => throw new IllegalArgumentException
    case x :: xs => {
      val nextPMessage = source.value.find(_._1.name == x).getOrElse(throw new IllegalArgumentException)._2
      if(xs.isEmpty) nextPMessage else getByPath(xs, nextPMessage.asInstanceOf[PMessage])
    }
  }

  private def setByPath(path: Path, value: PValue, destination: PMessage): PMessage = path match {
    case Nil => throw new IllegalArgumentException
    case x :: xs => {
      val descriptor = destination.value.find(_._1.name == x).get
      destination.copy(
        value = destination.value.updated(
          descriptor._1,
          if(xs.isEmpty || descriptor._2 == PEmpty) value else setByPath(xs, value, descriptor._2.asInstanceOf[PMessage]) // ?TODO Refactor
        )
      )
    }
  }

  override def toString(): String = toFieldMask.toSingleString
}

object FieldMaskTree {

  def apply(fieldMask: FieldMask): FieldMaskTree =
    FieldMaskTree(mergeFromFieldMask(FieldMaskTree(), fieldMask).root)

  def mergeFromFieldMask(fieldMaskTree: FieldMaskTree, fieldMask: FieldMask): FieldMaskTree = {
    fieldMask.paths.foldLeft(fieldMaskTree)((acc, b) => acc.addFieldPath(b))
  }

}
