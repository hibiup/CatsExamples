package com.hibiup.cats

import org.scalatest.FlatSpec

class Example_10_Monad_test extends FlatSpec{
    import Example_10_Monad._

    "Cats Monad definition" should "" in {
        cats_monad()
        cats_monad_syntax()
    }

    "Option Monad" should "" in {
        option_monad()
    }

    "Future Monad" should "" in {
        future_monad()
    }
}
