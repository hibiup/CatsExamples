package com.hibiup.cats

import org.scalatest.FlatSpec

class Example_9_Functor_Variant_test extends FlatSpec{
    "Contravariant functor" should "" in {
        import Example_9_Functor_Variant._
        contravariant()
        invariant()
    }
}
