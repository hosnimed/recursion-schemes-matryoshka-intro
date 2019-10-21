import demo.Trees.Tree._
import demo.Trees._
import matryoshka.data.Fix
import matryoshka.implicits._
import org.scalatest.FunSuite

class TreesSpec extends FunSuite {
  val fixTree: Fix[TreeF] = Fix[TreeF](
    BranchF(
      1,
      Fix[TreeF](LeafF(2)),
      Fix[TreeF](
        BranchF(
          3,
          Fix[TreeF](LeafF(4)),
          Fix[TreeF](LeafF(5))
        )
      )
    ))

  test("should fold a structure to int") {
    val r: Int = fixTree.cata[Int](treeAlg)
    assert(r === 15)
  }

  test("should fold a structure to string") {
    val s: String = fixTree.cata[String](showAlg)
    println(s"TreeToStringAlg: (Left to Right) == $s")
    assert(s === "(2 + 1 + (4 + 3 + 5))")
  }

  test("should un-fold a structure") {
    val ana: Fix[TreeF] = 4.ana.apply[Fix[TreeF]](treeCoAlg)
    val expectedTree: Fix[TreeF] = BranchF(4, LeafF[Fix[TreeF]](3).embed, BranchF(2, LeafF[Fix[TreeF]](1).embed, EmptyF[Fix[TreeF]]().embed).embed).embed
    assert(ana === expectedTree)
  }

  test("should re-fold a structure") {
    val hylo = 10.hylo(treeAlg, treeCoAlg)
    assert(hylo == (0 to 10).sum)
  }

}
