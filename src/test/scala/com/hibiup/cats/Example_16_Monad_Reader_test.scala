package com.hibiup.cats

import org.scalatest.FlatSpec

import Example_16_Monad_Reader._
class Example_16_Monad_Reader_test extends FlatSpec{
    "Reader Monad" should "" in reader_example

    "Reader map and flatMap" should "" in reader_map_and_flatMap

    it should "read from database" in read_form_database
}
