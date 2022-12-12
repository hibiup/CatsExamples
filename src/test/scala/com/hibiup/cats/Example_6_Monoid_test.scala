package com.hibiup.cats

import org.scalatest.flatspec.AnyFlatSpec
import cats.syntax.all._

import scala.util.{Failure, Success, Try}

class Example_6_Monoid_test extends AnyFlatSpec{
    "Semigroup" should "" in {
        import Example_6_Monoid_1._
        val intMonoid = IntMonoid.apply()
        assert(intMonoid.combine(2, 1) == intMonoid.combine(1, 2))
    }

    "Cats Monoid" should "" in {
        import Example_6_Cats_Monoid._
        string_monoid()
        option_monoid()
        monoid_syntax()
        sum_a_list_with_moniod()
        customized_monoid()
    }

    "Option Monoid" should "" in {
        val a = Option(1)
        val b = Option(2)
        println(a |+| b) // Some(3)
    }

    "Option MonoidK" should "" in {
        val a = Option(1)
        val b = Option(2)
        println( a <+> b )  // Some(1)
    }

    "Try Monoid" should "" in {
        val t1 = Try(1)
        val t2 = Try(2)
        println(t1 |+| t2)  // Success(3)

        // Invalid
        // println(t1 <+> t2)
    }

    "Either Monoid and MonoidK" should "" in {
        val r1: Either[String, Int] = Right(1)
        val r2: Either[String, Int] = Right(2)
        println(r1 |+| r2)  // Right(1+2) = Right(3)
        println(r1 <+> r2)  // Right(1) or Right(2) = Right(1)

        // |+|: Left is period
        val la: Either[String, Int] = Left("A")
        val lb: Either[String, Int] = Left("B")
        println(la |+| lb)  // Left(A)
        println(lb |+| la)  // Left(B)
        println(r1 |+| la)  // Left(A)
        println(la |+| r1)  // Left(A)

        println(la <+> lb)  // Left(B)
        println(r1 <+> la)  // Right(1)
        println(la <+> r1)  // Right(1)
    }
}
