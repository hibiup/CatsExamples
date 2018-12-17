package com.hibiup.cats

import org.scalatest.FlatSpec

class Example_8_Future_Future_test extends  FlatSpec{
    "Future Functor" should "" in {
        import Example_8_Future_Functor._
        future_map
        future_can_only_run_once
    }
}
