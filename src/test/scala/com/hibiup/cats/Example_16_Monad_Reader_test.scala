package com.hibiup.cats

import org.scalatest.flatspec.AnyFlatSpec

import Example_16_Monad_Reader._
class Example_16_Monad_Reader_test extends AnyFlatSpec{
    "Reader Monad" should "" in reader_example

    "Reader map and flatMap" should "" in reader_map_and_flatMap

    it should "read from database" in read_form_database
}
