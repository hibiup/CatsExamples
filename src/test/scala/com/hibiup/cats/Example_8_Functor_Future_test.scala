package com.hibiup.cats

import org.scalatest.flatspec.AnyFlatSpec

class Example_8_Functor_Future_test extends  AnyFlatSpec{
    "Future Functor" should "" in {
        import Example_8_Functor_Future._
        future_map
        future_can_only_run_once
    }
}
