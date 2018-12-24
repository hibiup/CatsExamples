package com.hibiup.cats

import org.scalatest.FlatSpec

class Example_14_Eval_test extends FlatSpec{
    "Cats Eval basic usage" should "" in {
        import Example_14_Eval._
        cats_eval()
        eval_greeting()
    }

    "Cats Eval recursive" should "be stack safe" in {
        import Example_14_Eval._
        println(safe_factorial(10000).value)
    }
}
