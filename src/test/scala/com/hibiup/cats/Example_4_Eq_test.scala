package com.hibiup.cats

import org.scalatest.FlatSpec

class Example_4_Eq_test extends FlatSpec{
    "Cats Eq" should "" in {
        import com.hibiup.cats.Example_4_Eq._
        compare_int()
        compare_option()
        compare_list()
        compare_customized_class()
    }

}
