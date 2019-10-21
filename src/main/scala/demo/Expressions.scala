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
    implicit val expFunctor = TODO

    /**
      * Algebra: Expr[Int] => Int
      */
    def exprAlg = TODO

    /**
      * CoAlgebra: Int => Expr[Int]
      */
    def expCoAlg = TODO

    /**
      * Algebra: Expr[String] => String
      */
    def showAlg = TODO
  }

}