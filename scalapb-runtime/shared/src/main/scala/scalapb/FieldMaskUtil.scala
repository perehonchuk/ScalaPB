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

  def intersection (mask1: FieldMask, mask2: FieldMask): FieldMask = {
    FieldMaskTree(mask1).intersect(FieldMaskTree(mask2)).toFieldMask
  }

  def toJsonString(fieldMask: FieldMask): String = {
    val buf   = new java.lang.StringBuilder()
    var first = true
    fieldMask.paths.foreach { path =>
      if (!path.isEmpty) {
        if (!first) {
          buf.append(',')
        }
        lowerSnakeCaseToCamelCaseWithBuffer(path, buf)
        first = false
      }
    }
    buf.toString
  }

  def fromJsonString(value: String): FieldMask = {
    val result = value
      .split(",")
      .toIterator
      .withFilter(_.nonEmpty)
      .map { path =>
        camelCaseToSnakeCase(path)
      }
      .toList
    FieldMask(result)
  }

  private[this] def isLower(c: Char): Boolean = {
    'a' <= c && c <= 'z'
  }

  private[this] def isUpper(c: Char): Boolean = {
    'A' <= c && c <= 'Z'
  }

  private[this] def toUpper(c: Char): Char = {
    if (isLower(c)) {
      (c - 32).asInstanceOf[Char]
    } else {
      c
    }
  }

  private[this] def toLower(c: Char): Char = {
    if (isUpper(c)) {
      (c + 32).asInstanceOf[Char]
    } else {
      c
    }
  }

  private[this] def toLowerCase(s: String, b: Appendable): Unit = {
    @tailrec
    def loop(i: Int): Unit = {
      if (i < s.length) {
        b.append(toLower(s(i)))
        loop(i + 1)
      }
    }

    loop(0)
  }

  private[scalapb] def lowerSnakeCaseToCamelCase(name: String): String = {
    lowerSnakeCaseToCamelCaseWithBuffer(name, new java.lang.StringBuilder(name.length)).toString
  }

  private[scalapb] def lowerSnakeCaseToCamelCaseWithBuffer(
                                                            name: String,
                                                            buf: Appendable
                                                          ): buf.type = {
    def toProperCase(s: String): Unit = if (!s.isEmpty) {
      buf.append(toUpper(s(0)))
      toLowerCase(s.substring(1), buf)
    }

    val array = name.split("\\_")
    toLowerCase(array(0), buf)

    @tailrec
    def loop(i: Int): Unit = {
      if (i < array.length) {
        toProperCase(array(i))
        loop(i + 1)
      }
    }

    loop(1)
    buf
  }

  private[scalapb] def camelCaseToSnakeCase(str: String): String = {
    if (str.isEmpty) {
      ""
    } else {
      val buf = new java.lang.StringBuilder(str.length)
      buf.append(toLower(str(0)))

      @tailrec
      def loop(i: Int): String = {
        if (i < str.length) {
          val c = str(i)
          if (isUpper(c)) {
            buf.append('_')
            buf.append((c + 32).asInstanceOf[Char])
          } else {
            buf.append(c)
          }
          loop(i + 1)
        } else {
          buf.toString
        }
      }

      loop(1)
    }
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
