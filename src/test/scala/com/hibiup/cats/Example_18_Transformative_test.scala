package com.hibiup.cats

import org.scalatest.FlatSpec

import Example_18_Transformative._

class Example_18_Transformative_test extends FlatSpec{
    "Transformative example" should "convert OptionT[List, A] to ListOption[A]" in transformative_example
    "Either[String, A]" should "be able to work with OptionT[EitherOr, A]" in either_option_transformer
}
