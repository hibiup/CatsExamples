package com.hibiup.cats

import org.scalatest.FlatSpec

class ShowCatsTest extends FlatSpec{

    "A show cats" should "" in {
        import com.hibiup.cats.Cats.Show

        val showInt = Show.apply[Int]
        val r: String = showInt.show(1212312)
        println(r)
    }
}
