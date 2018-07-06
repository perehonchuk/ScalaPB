package scalapb

import scalapb.descriptors.{Descriptor, PMessage, ScalaType}
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

  def merge[T <: GeneratedMessage](mask: FieldMask, source: T, destination: T): T = {
    destination.companion.messageReads.read(
      FieldMaskTree(mask).merge(mask, source.toPMessage, destination.toPMessage)
    ).asInstanceOf[T]
  }

  def merge(mask: FieldMask, source: PMessage) = {
    FieldMaskTree(mask).merge(mask, source)
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
