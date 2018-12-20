package com.hibiup.cats

import org.scalatest.FlatSpec

class Example_10_Monad_test extends FlatSpec{
    "Option Monad" should "" in {
        import Example_10_Monad._
        option_monad()
    }

    "Future Monad" should "" in {
        import Example_10_Monad._
        future_monad()
    }
}
