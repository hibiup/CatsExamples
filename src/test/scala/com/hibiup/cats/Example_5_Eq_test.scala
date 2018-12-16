package com.hibiup.cats

import org.scalatest.FlatSpec

class Example_5_Eq_test extends FlatSpec{
    "Cats Eq" should "" in {
        import com.hibiup.cats.Example_5_Eq._
        compare_int()
        compare_option()
        compare_customized_class()
    }

}
