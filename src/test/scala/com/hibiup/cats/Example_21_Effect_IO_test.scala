package com.hibiup.cats

import org.scalatest.FlatSpec

class Example_21_Effect_IO_test extends FlatSpec{
    import Example_21_Effect_IO.stack_safe._

    "Cats Effect IO" should "" in fib(100).unsafeRunSync
}
