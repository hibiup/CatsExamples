package com.hibiup.cats

import org.scalatest.FlatSpec

import Example_17_Monad_State._
class Example_17_Monad_State_test extends FlatSpec{
    "state monad" should "" in state_example
    it should "be able to compose and transform" in state_composing_and_transforming
}
