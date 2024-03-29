package com.hibiup.cats

import Example_17_Monad_State._
import org.scalatest.flatspec.AnyFlatSpec

class Example_17_Monad_State_test extends AnyFlatSpec{
    "state monad" should "" in state_example
    it should "be able to compose and transform" in state_composing_and_transforming

    "Increase and return old value" should "" in {
        import cats.data.State

        case class Json(data: Int)
        val nextJson: State[Json, Option[Int]] = State { j =>
            (Json(j.data + 1), Option(j.data))
        }

        val a = (for {
            _ <- nextJson // _==1
            n <- nextJson // n==2
        } yield n).run(Json(1)).value

        assert(a._2.contains(2))  // a == (json(3), Some(2))
    }
}
