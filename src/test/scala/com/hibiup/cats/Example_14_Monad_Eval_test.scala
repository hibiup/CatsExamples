package com.hibiup.cats

import org.scalatest.FlatSpec

class Example_14_Monad_Eval_test extends FlatSpec{
    "Cats Eval basic usage" should "" in {
        import Example_14_Eval._
        cats_eval()
        eval_greeting()
    }

    "Cats Eval recursive" should "be stack safe" in {
        import Example_14_Eval_trampoline._
        println(safe_factorial(10000).value)

        println(foldRight((1 to 10000).toList, 0)(_ + _))
    }
}
