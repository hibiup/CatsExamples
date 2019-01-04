package com.hibiup.cats

/**
  * Monoid 的定义：
  * */
object Example_6_Monoid_1 {
    /**
      * 半群：满足结合率和交换率
      * */
    trait Semigroup[A] {
        def combine(x: A, y: A): A
        val associative = combine(_,_)
    }

    /**
      * Monoid: 幺半群，在半群的基础上含有幺元
      * */
    trait Monoid[A] extends Semigroup[A] {
        def empty: A
        def identity = empty
        def zero = empty
    }

    /** 实例化一个 Int Monoid */
    object IntMonoid {
        def apply() = new Monoid[Int] {
            override def empty: Int = 0
            override def combine(x: Int, y: Int): Int = x + y
        }
    }
}

/**
  * Cats 提供了缺省的 Monoid typeclass, 预定义了以上方法, 通过直接实现相应的函数接口就可以获得 Monoid
  * */
object Example_6_Cats_Monoid {
    import cats.syntax.eq._

    def string_monoid() {
        /** 1）引进 Cats 缺省实现的 string monoid */
        import cats.instances.string._

        /** 2）直接使用 String Monoid */
        import cats.Semigroup
        assert("Hi there" === Semigroup[String].combine("Hi ", "there")) // res0: String = Hi there

        // Monoid 的 combine 继承自 Semigroup
        import cats.Monoid
        assert("Hi there" === Monoid[String].combine("Hi ", "there")) // res0: String = Hi there
        assert("" === Monoid[String].empty) // res1: String = ""
    }

    def option_monoid() {
        import cats.Monoid
        import cats.instances.int._
        import cats.instances.option._

        val a = Option(22)
        val b = Option(33)
        assert(Option(55) === Monoid[Option[Int]].combine(a, b))
    }

    def monoid_syntax() {
        import cats.Monoid

        /** 1）引进 String Monoid syntax*/
        import cats.syntax.semigroup._    // 包含 |+| 运算符

        import cats.instances.string._
        assert("Hi there" === ("Hi " |+| "there" |+| Monoid[String].empty))

        import cats.instances.int._
        assert(3 === (1 |+| 2 |+| Monoid[Int].empty))
    }

    def sum_a_list_with_moniod(): Unit = {
        import cats.Monoid
        import cats.syntax.semigroup._

        /** 利用 context bound 来声明 add, 类型参数 [A:Monoid] 表示在函数中隐式声明一个 Monoid[A] 实例.  */
        def add[A:Monoid](l: List[A]):A =
            /** 所以我们可以在代码中用 implicitly[Monoid[A]] 来捕获, 甚至连 implicitly 都可以省略 */
            l.foldLeft(Monoid[A].empty)(_ |+| _)

        /** 支持数字累加 */
        import cats.instances.int._
        assert(6 === add(List(1,2,3)))

        /** 也支持 Option[Int] */
        import cats.instances.option._
        assert(Option(6) === add(List(Option(1), Option.empty, Option(2),Option(3))))
    }

    /** 对于定制类 Monoid A, 只要继承 Monad[A], 然后实现相关方法即可以方便地使用: */
    def customized_monoid() {
        /** 例如我们希望定制类 Order 也能够方便地使用 foldLeft Monoid 方法. */
        case class Order(totalCost: Double, quantity: Double)

        /** 1) 声明 Monoid 的通用方法, 通过 context bound 获得具体 Monoid */
        import cats.Monoid
        import cats.syntax.semigroup._
        def add[A:Monoid](l: List[A]):A = l.foldLeft(Monoid[A].empty)(_ |+| _)

        /** 2) 为上下文隐式实现 Order 的 empty 和 combine 方法 */
        implicit object Order extends Monoid[Order] {
            override def empty: Order = Order(0,0)
            override def combine(x: Order, y: Order): Order = Order(x.totalCost+y.totalCost, x.quantity+y.quantity)
        }

        /** 定制 Eq */
        import cats.Eq
        import cats.instances.double._
        implicit val orderEqual: Eq[Order] =
            Eq.instance[Order] { (o1, o2) =>
                (o1.totalCost === o2.totalCost) &&
                        (o1.quantity === o2.quantity)
            }

        /** 3) 就可以直接对 Order 使用 add 了 */
        assert(Order(15, 2) === add(List(Order(10, 1), Order.empty, Order(5,1))))
    }
}