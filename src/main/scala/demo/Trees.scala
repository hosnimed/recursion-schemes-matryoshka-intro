package demo

import matryoshka.{Algebra, Coalgebra}
import scalaz.Functor

object Trees {
  /**
    * Explicit recursion tree ADT
    */
  sealed trait Tree
  final case class Branch(label: Int, left: Tree, right: Tree) extends Tree
  final case class Leaf(label: Int)                            extends Tree
  final case class Empty()                                     extends Tree

  /**
    * So the first thing to do is to "translate" our Tree to a pattern-functor.
    * This is done by adding a type parameter and replace each recursive occurrences
    * of Tree by this type parameter in the ADT.
    */
  sealed trait TreeF[+A]
  case class BranchF[A](v: Int, v1: A, v2: A) extends TreeF[A]
  case class LeafF[A](v: Int)                 extends TreeF[A]
  case class EmptyF[A]()                      extends TreeF[A]

  object Tree {
    /**
      * [[scalaz.Functor]] instance for our structure
      */
    implicit val treeFunctor =  new Functor[TreeF] {
      def map[A, B](fa: TreeF[A])(f: A => B): TreeF[B] = fa match {
        case BranchF(v, v1, v2) => BranchF(v, f(v1), f(v2))
        case LeafF(v) => LeafF(v)
        case EmptyF() => EmptyF()
      }
    }
    /**
      * Algebra: Tree[Int] => Int
      */
    def treeAlg: Algebra[TreeF, Int] = {
      case BranchF(v, v1, v2) => v1 + v + v2
      case LeafF(v) => v
      case EmptyF() => 0
  }
    /**
      * CoAlgebra: Int => Tree[Int]
      */
    def treeCoAlg: Coalgebra[TreeF, Int] =
      (n: Int) => {
        n match {
          case 0 => EmptyF()
          case 1 => LeafF(1)
          case _ if n % 2 == 0 => BranchF(n, n - 1, n - 2)
          case _ => LeafF(n)
        }
      }
    /**
      * Algebra: Tree[String] => String
      */
    def showAlg: Algebra[TreeF, String] = {
      case EmptyF()           => "0"
      case LeafF(v)           => s"$v"
      case BranchF(v, v1, v2) => s"($v1 + $v + $v2)"
    }

  }

}