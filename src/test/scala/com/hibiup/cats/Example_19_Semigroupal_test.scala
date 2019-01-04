package com.hibiup.cats

import org.scalatest.FlatSpec

class Example_19_Semigroupal_test extends FlatSpec{
    import Example_19_Semigroupal._

    "Semigroupal is a package from Cats" should "provides Semigroups features" in semigroupal_examples

    "Semigroup Future" should "run mulitiple thread" in semigroupal_future_executes_paramelly

    "Semigroup Either" should "stop at first Left" in Semigroupal_either
}
