package com.hibiup.cats

import org.scalatest.flatspec.AnyFlatSpec

import Example_18_Transformative._

class Example_18_Transformative_test extends AnyFlatSpec{
    "Transformative example" should "convert OptionT[List, A] to ListOption[A]" in transformative_example
    "Either[String, A]" should "be able to work with OptionT[EitherOr, A]" in either_option_transformer
    "Future[Either[_,_]]" should "works asynchronously" in future_either_transformer
}
