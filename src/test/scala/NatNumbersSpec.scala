import demo.NatNumbers.Nat.{intToNatCoAlg, natToIntAlg, natToStringAlg}
import demo.NatNumbers._
import matryoshka.data.Fix
import matryoshka.implicits._
import org.scalatest.{FunSuite, Matchers}

class NatNumbersSpec extends FunSuite{
  //  nat fix
  val nat: Fix[Nat] = Fix(Succ(Fix(Succ(Fix(Succ(Fix[Nat](Zero())))))))

  test("(1) should fold a structure to int") {
    //  cata : nat => int
    val r: Int = nat.cata[Int](natToIntAlg)
    assert(r === 3)
  }
  test("(2) should fold a structure to int") {
    //  cata : nat => int
    val r: Int = Nat.natToInt(nat)
    assert(r === 3)
  }

  test("(1) should fold a structure to string") {
    val s: String = nat.cata[String](natToStringAlg)
    println(s"NatToStringAlg == $s")
    assert(s === "Succ(Succ(Succ(0)))")
  }
  test("(2) should fold a structure to string") {
    val s: String = Nat.natToString(nat)
    println(s"NatToStringAlg == $s")
    assert(s === "(1 + (1 + (1 + 0)))")
  }

  test("should un-fold a structure") {
    //  ana : int => fix[nat]
    val ana: Fix[Nat] = 3.ana.apply[Fix[Nat]](intToNatCoAlg)
    assert(ana === nat)
  }

  test("should re-fold a structure") {
    //  int =[ana]=> nat [hylo] nat =[cata]=>  int
    val hylo = 3.hylo(natToIntAlg, intToNatCoAlg)
    assert(hylo === 3)
  }

}
