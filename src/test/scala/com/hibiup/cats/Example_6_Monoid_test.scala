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

        // Monoid: Some(A + B)
        println(a |+| b) // Some(3)

        val n:Option[Int] = None
        println(a |+| n)  // Some(1 + Zero) = Some(1)
        println(n |+| a)  // Some(Zero + 1) = Some(1)
    }

    "Option MonoidK" should "" in {
        val a = Option(1)
        val b = Option(2)

        // MonoidK
        // Option 和 Either 的 <+> 是 OR 运算: first valid value(First Some OR None)
        println( a <+> b ) // Some(1) OR Some(2) = Some(1)

        val n:Option[Int] = None
        println(a <+> n)  // Some(1) + Zero = Some(1)
        println(n <+> a)  // Zero + Some(1) = Some(1)

        val n1:Option[Int] = None
        println(n |+| n1) // Option(Zero + Zero) = None
        println(n <+> n1) // Zero + Zero = None
    }

    "Try Monoid" should "" in {
        val t1:Try[Int] = Try(1)
        val t2:Try[Int] = Success(2)

        // Monoid
        println(t1 |+| t2)  // Try(1 + 2) = Success(3)

        // MonoidK. Invalid ???
        //println(t1 <+> t2)

        val f1: Try[Int] = Failure(new RuntimeException(("A")))
        println(f1 |+| t2)  // Try(_ + Throwable) = Failure
        println(t1 |+| f1)  // Try(Throwable + _) = Failure
    }

    "Either Monoid and MonoidK" should "" in {
        val r1: Either[String, Int] = Right(1)
        val r2: Either[String, Int] = Right(2)

        println(r1 |+| r2)  // Right(1+2) = Right(3)
        // Option 和 Either 的 <+> 是 OR 运算。
        println(r1 <+> r2)  // Right(1) or Right(2) = Right(1)

        val la: Either[String, Int] = Left("A")
        val lb: Either[String, Int] = Left("B")

        // Monoid: "Left" is period, if both "Left", return the first one. Byt why?
        println(la |+| lb)  // Left(A)
        println(lb |+| la)  // Left(B)
        println(r1 |+| la)  // Left(A)
        println(la |+| r1)  // Left(A)

        // MonoidK: ???
        println(la <+> lb)  // Left(B)
        println(r1 <+> la)  // Right(1)
        println(la <+> r1)  // Right(1)
    }

    "Map Monoid" should "" in {
        val m1 = Map("A" -> 1)
        val m2 = Map("A" -> 1)

        // Monoid
        println(m1 |+| m2) // Map(_ + _) = Map(A -> (1+1)) = Map(A -> 2)

        // MonoidK
        // 注意：Collection 的 <+> 是"并集"运算，和 Option，Either 不同。
        println(m1 <+> m2) // Map1 + Map2 = Map(A -> 1) union Map(A -> 1) = Map(A -> 1)

        //
        val m0 = Map[String, Int]()
        println(m0 <+> m2) // Zero + Map(A -> 1) = Map(A -> 1)
        println(m0 |+| m2) // Map(A -> Zero + 1) = Map(A -> 1)

        val m3 = Map("C" -> 3)
        println(m1 |+| m3) // Map((A -> 1) + (C -> 3)) = Map(A -> 1, C -> 3)
        println(m1 <+> m3) // Map1 + Map3 = Map(A ->1) union Map(C -> 3) = Map(A -> 1, C ->3)
    }

    "List Monoid" should "" in {
        val l1 = List(1, 2)
        val l2 = List(3, 4)

        // Monoid
        println(l1 |+| l2) // List(1, 2, + 3, 4) = List(1,2,3,4)

        // MonoidK
        // 注意：Collection 的 <+> 是"并集"运算，和 Option，Either 不同。
        println(l1 <+> l2) // List(1, 2) + List(3, 4) = List(1,2,3,4)

        //
        val l0 = List[Int]()
        println(l0 |+| l1) // List(Zero + 1,2) = List(1,2)
        println(l0 <+> l1) // Zero + List(1,2) = List(1,2)
    }
}
