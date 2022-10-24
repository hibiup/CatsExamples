package com.hibiup.cats

import org.scalatest.flatspec.AnyFlatSpec

class Example_4_Eq_test extends AnyFlatSpec{
    "Cats Eq" should "" in {
        import com.hibiup.cats.Example_4_Eq._
        compare_int()
        compare_option()
        compare_list()
        compare_customized_class()
    }

}
