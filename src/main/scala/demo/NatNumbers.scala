package demo

import matryoshka.{Algebra, Coalgebra, Recursive}
import scalaz.Functor

object NatNumbers {
  /**
    * Pattern-functor ADT
    */
  sealed trait Nat[+A]
  case class Succ[A](previous: A) extends Nat[A]
  case class Zero[A]()            extends Nat[A]

  object Nat {
    /**
      * [[scalaz.Functor]] instance for our structure
      */
    implicit val natFunctor = TODO

    /**
      * Algebra: Nat[Int] => Int
      */
    def natToIntAlg: Algebra[Nat, Int] = TODO
    /**
      * CoAlgebra: Int => Nat[Int]
      */
    def intToNatCoAlg: Coalgebra[Nat, Int] = TODO
    def natToStringAlg: Algebra[Nat, String] = TODO

    /**
      * Recursive.Aux[T, Nat] : Another way to `collapse` our structure
      */
    import matryoshka.implicits._
    def natToInt[T](nat: T)(implicit T: Recursive.Aux[T, Nat]): Int = nat.cata[Int] {
      case Zero()         => 0
      case Succ(previous) => 1 + previous
    }
    def natToString[T](nat: T)(implicit T: Recursive.Aux[T, Nat]): String = nat.cata[String] {
      case Zero()         => "0"
      case Succ(previous) => s"(1 + $previous)"
    }
  }
}
