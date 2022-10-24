package com.hibiup.cats

import org.scalatest.flatspec.AnyFlatSpec

class Example_13_Monad_Either_test extends AnyFlatSpec{
    "Cats Either syntax" should "" in {
        import Example_13_Monad_Either._
        cats_either()
    }

    "Monad Error handling" should "" in {
        import Example_13_Monad_Either._
        monad_error()
    }
}
