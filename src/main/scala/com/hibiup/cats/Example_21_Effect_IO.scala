package com.hibiup.cats

import cats.effect.IO

object Example_21_Effect_IO {
    def fib(n: Int, a: Long=1, b: Long=1): IO[Long] =
        IO.suspend {
            if (n > 0) {
                println(s"Thread-${Thread.currentThread().getId}")
                fib(n - 1, b, a + b)
            }
            else {
                println(s"Thread-${Thread.currentThread().getId}")
                IO.pure(a)
            }
        }
}
