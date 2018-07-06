
import com.trueaccord.proto.e2e.sealed_oneof._
import com.trueaccord.proto.e2e.{sealed_oneof_single_file => f}

import org.scalatest._

class SealedOneofSpec extends FlatSpec with MustMatchers {

  val expr = Add(Lit(1), Add(Lit(2), Lit(3)))

  "Expr.toExprMessage.toExpr" should "roundtrip" in {
    assert(expr == expr.asMessage.toExpr)
  }

  "ExprMessage.toByteArray" should "work via Expr" in {
    val expr2  = ExprMessage.parseFrom(expr.asMessage.toByteArray).toExpr
    assert(expr == expr2)
  }

  "fields of sealed_oneof type" should "default Empty" in {
    assert(Add() == Add(Expr.Empty, Expr.Empty))
  }

  "fields of repeated sealed_oneof type" should "work like normal" in {
    val programs = Programs(programs = List(expr, expr), optionalExpr = expr, exprMap = Map("44" -> expr))
    val programs2 = Programs.parseFrom(programs.toByteArray)
    assert(programs == programs2)
  }

  trait UnsealedExpr

  "Expr" should "be sealed" in {
    assertCompiles("class Foo extends UnsealedExpr")
    assertTypeError("class Foo extends Expr")
  }

  "single_file=true" should "work with sealed_oneof" in {
    val fexpr = f.Add(f.Lit(1), f.Add(f.Lit(2), f.Lit(3)))
    assert(
      fexpr.asMessage.toProtoString ==
      expr.asMessage.toProtoString
    )
  }
}

