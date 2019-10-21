package demo

import matryoshka.{Algebra, Coalgebra}
import scalaz.Functor

object Expressions {

  /**
    * Pattern-functor ADT
    */
  sealed trait Expr[+A]
   case class Value[A](v: Int) extends Expr[A]
  case class Add[A](v1: A, v2: A) extends Expr[A]

  object Expr {

    /**
      * [[scalaz.Functor]] instance for our structure
      */
    implicit val expFunctor = new Functor[Expr] {
      def map[A, B](fa: Expr[A])(f: A => B): Expr[B] = fa match {
        case Value(v) => Value(v)
        case Add(v1, v2) => Add(f(v1), f(v2))
      }
    }

    /**
      * Algebra: Expr[Int] => Int
      */
    def exprAlg : Algebra[Expr, Int] = {
      case Value(v) => v
      case Add(v1, v2) => v1 + v2
    }

    /**
      * CoAlgebra: Int => Expr[Int]
      */
    def expCoAlg: Coalgebra[Expr, Int] = (n:Int) => n match {
      case 0 => Value(0)
      case 1 => Value(1)
      case _ if n % 2 == 0 => Add(n-1, n-2)
      case _ => Value(n)
    }

    /**
      * Algebra: Expr[String] => String
      */
    def showAlg: Algebra[Expr, String] = {
      case Value(v) => s"$v"
      case Add(v1, v2) => s"($v1 + $v2)"
    }
  }

}