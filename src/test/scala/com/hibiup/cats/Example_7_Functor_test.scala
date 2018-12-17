package com.hibiup.cats

import org.scalatest.FlatSpec

class Example_7_Functor_test extends FlatSpec{
    "Functor map test" should "" in {
        import com.hibiup.cats.Example_7_Functor_map._
        functor_map()
        functor_syntax()
    }

    "Functor compose test" should "" in {
        import com.hibiup.cats.Example_7_Functor_2._
        println(functor)
    }

    "Functor in Functor test" should "" in {
        /*def func1[A, B](a:A)(b:A=>B):(B,B) = (b,b)
        def func2[C](c:C) = func1(c)(_ => _)
        func2(1)(2)*/

        import com.hibiup.cats.Example_7_Functor_3._
        println(mapper)
        println(r)
    }
}
