package com.hibiup.cats

import org.scalatest.FlatSpec

class Example_20_Free_test extends FlatSpec {
    import Example_20_Free._
    "Free monad" should "" in free_monad_example
    "composing monad" should "" in composing_free
    "FreeT monad" should "" in freeT_example
}
