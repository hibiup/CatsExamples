package com.hibiup.cats

import org.scalatest.flatspec.AnyFlatSpec

import Example_15_Monad_Writer._
class Example_15_Monad_Writer_test extends AnyFlatSpec{
    "Writer Monad" should "" in writer_example

    it should "support flatMap and map methods" in writer_monad_computation

    it should "works with customized class" in writer_for_customized_type

    it should "be thread safe" in writer_in_multiple_thread

    "Prompt example" should "" in writer_for_greeting
}
