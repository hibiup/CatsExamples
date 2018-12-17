package com.hibiup.cats

import org.scalatest.FlatSpec

class Example_7_Monoid_test extends FlatSpec{
    "Semigroup" should "" in {
        import Example_7_Monoid_1._
        val intMonoid = IntMonoid.apply()
        assert(intMonoid.combine(2, 1) == intMonoid.combine(1, 2))
    }

    "Cats Monoid" should "" in {
        import Example_7_Cats_Monoid._
        string_monoid()
        option_monoid()
        monoid_syntax()
        sum_a_list_with_moniod()
    }
}
