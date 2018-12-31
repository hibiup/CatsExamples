package com.hibiup.cats

import org.scalatest.FlatSpec

class Example_10_Monad_test extends FlatSpec{
    import Example_10_Monad._

    "Cats Monad definition" should "" in {
        cats_monad()
        cats_monad_syntax()
    }

    "Option Monad" should "" in option_monad()
    "Custominzed monad" should "support tail recursive" in tailrec_example
    it should "be capable for Tree type" in customized_monad_example
}
