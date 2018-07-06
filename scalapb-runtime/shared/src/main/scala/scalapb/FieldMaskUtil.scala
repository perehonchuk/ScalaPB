package scalapb

import scalapb.descriptors.{Descriptor, ScalaType}
import com.google.protobuf.field_mask.FieldMask

import scala.annotation.tailrec

object FieldMaskUtil {
  private val FIELD_PATH_SEPARATOR = ","
  private val FIELD_SEPARATOR_REGEX = "\\."

  def isValid(descriptor: Descriptor, fieldMask: FieldMask): Boolean = {
    fieldMask.paths.forall(isValid(descriptor, _))
  }

  def isValid(descriptor: Descriptor, path: String): Boolean = {
    val parts = path.split(FIELD_SEPARATOR_REGEX)
    isValid(descriptor, parts)
  }

  def intersection (mask1: FieldMask, mask2: FieldMask): FieldMask = {
    FieldMaskTree(mask1).intersect(FieldMaskTree(mask2)).toFieldMask
  }

  @tailrec
  private def isValid(descriptor: Descriptor, parts: Array[String]): Boolean = {
    parts match {
      case Array() => false
      case Array(head) => descriptor.findFieldByName(head).isDefined
      case _ => descriptor.findFieldByName(parts.head) match {
        case Some(f) => if (f.isRepeated) {
          false
        } else {
          messageDescriptor(f.scalaType) match {
            case Some(desc) => isValid(desc, parts.tail)
            case None => false
          }
        }
        case None => false
      }
    }
  }

  private def messageDescriptor(scalaType: ScalaType): Option[Descriptor] = {
    scalaType match {
      case ScalaType.Message(descriptor) => Some(descriptor)
      case _ => None
    }
  }

  implicit class FieldMaskExtensions(val fieldMask: FieldMask) extends AnyVal {
    def toSingleString: String =
      fieldMask.paths.mkString(FIELD_PATH_SEPARATOR)
  }
}
