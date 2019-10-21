import demo.Expressions.Expr._
import demo.Expressions._
import matryoshka.data.Fix
import matryoshka.implicits._
import org.scalatest.FunSuite

class ExpressionsSpec extends FunSuite {
  val fixExp: Fix[Expr] = Fix[Expr](
    Add(
      Fix(Add(Fix[Expr](Value(5)), Fix[Expr](Value(2)))),
      Fix[Expr](Value(3))
    ))

  test("should fold a structure to int") {
    val r: Int = fixExp.cata[Int](exprAlg)
    assert(r === 10)
  }

  test("should fold a structure to string") {
    val s: String = fixExp.cata[String](showAlg)
    println(s"ExprToStringAlg: == $s")
    assert(s === "((5 + 2) + 3)")
  }

  test("should un-fold a structure") {
    val ana: Fix[Expr] = 6.ana.apply[Fix[Expr]](expCoAlg)
    val expectedExpr: Fix[Expr] =  Add(Value[Fix[Expr]](5).embed, Add(Value[Fix[Expr]](3).embed, Add(Value[Fix[Expr]](1).embed,Value[Fix[Expr]](0).embed).embed).embed).embed
    assert(ana === expectedExpr)
  }

  test("should re-fold a structure") {
    val hylo = 10.hylo(exprAlg, expCoAlg)
    assert(hylo ==  (0 to 10).filterNot(_ % 2 == 0).sum)
  }

}
