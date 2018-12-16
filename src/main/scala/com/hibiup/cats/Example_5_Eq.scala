package com.hibiup.cats

/**
  * Scala 的 == 运算是类型不安全的，比如：
  * 　　List(1, 2, 3).map(Option(_)).filter(item => item == 1)
  *　Scala 并不能发现实际上是在做 Option(1) == 1 比较。为此 Cats 提供了 Eq 来支持类型安全的比较运算。
  * */
object Example_5_Eq {
    /** cats.Eq 是类型安全的比较类 */
    import cats.Eq
    /** cats.syntax.eq._ 包含了包括 "===" 和 "=!=" 再内的隐式*/
    import cats.syntax.eq._

    /** 比较基本类型 */
    def compare_int(): Unit = {
        /**  首先要引进被比较对象的隐式转换 */
        import cats.instances.int._

        /** 可以直接使用 Eq 来做比较　*/
        val eqInt = Eq[Int]
        assert(eqInt.eqv(123, 123))

        /** 或使用隐式 */
        assert(123 === 123)
    }

    /** 比较容器类型 */
    def compare_option(): Unit = {
        import cats.instances.int._ // for Int
        import cats.instances.option._ // for Option

        assert(Option(1) =!= None)
        assert(Option(1) === Some(1))
        assert(Option.empty[Int] === None)

        assert((Some(1):Option[Int]) =!= None)
        assert((Some(1):Option[Int]) =!= Option.empty[Int])
        assert((Some(1):Option[Int]) === Option(1))

        assert((None:Option[Int]) =!= Option(1))
        assert((None:Option[Int]) =!= (Some(1):Option[Int]))
        assert((None:Option[Int]) === None)

        /** cats.syntax.option._ 包含了 some 和 none */
        import cats.syntax.option._
        assert(1.some =!= none[Int])
    }

    def compare_list(): Unit = {
        import cats.instances.list._
        import cats.instances.int._
        import cats.instances.option._

        assert(List(1,2) === List(1, 2))
        assert(List(Option(1), None) === List(Some(1), None))
        assert(List(List(Option(1), None), List(Option(2), None)) =!= List(List(Some(1), None), List(Some(2))))
    }

    /** 比较一个自定义的类型，比如： */
    def compare_customized_class(): Unit = {
        final case class Cat(name: String, age: Int, color: String)

        /** 需要自己实现 case class 的比较算法，然后将之声明为隐式：*/
        import cats.instances.option._ // for Option
        import cats.instances.int._ // for Option
        import cats.instances.string._ // for Option

        implicit val catEqual: Eq[Cat] =
            Eq.instance[Cat] { (cat1, cat2) =>
                (cat1.name === cat2.name ) &&
                        (cat1.age === cat2.age ) &&
                        (cat1.color === cat2.color)
            }

        /** 就可以在应用中使用了 */
        val cat1 = Cat("Garfield", 38, "orange and black")
        val cat2 = Cat("Heathcliff", 33, "orange and black")

        assert(cat1 =!= cat2)
        assert(cat1 === cat1)

        assert((Some(cat1):Option[Cat]) =!= (Option(cat2):Option[Cat]))
        assert((Some(cat1):Option[Cat]) === (Option(cat1):Option[Cat]))
    }
}
