package com.hibiup.cats

import cats.syntax.eq._

/**
  * 在 Scala 里的 type 必须对应一个具体的类型值，比如：
  *
  *    Int          // type
  *    List[Int]    // type
  *
  * 而 List 就被称为 Type Constructor，它接受类型参数，产生出 type
  *
  *    List     // type constructor, 不是 type.
  *
  * kind 则是对 type constructor 的描述。例如：一个 Type constructor 描述的是 A -> B 的关系，那么它的 kind 就是 A -> B，也
  * 就是说 kind 是 type constructor 的 type，而实现这个关系的类本身是 value，比如 List 是一个 value, 不是 type. 所以 kind
  * 也被称为 type of type。三者之间的关系如下：
  *
  *    type constructor[type:kind]: kind -> value: type
  *
  * type constructor 是一个高级语言特性，我们需要在 build.sbt 中使用 scalacOptions += "-language:higherKinds" 来告诉编译器
  * 以免警告，或在代码中 import scala.language.higherKinds 。
  */

/**
  * Functor syntax
  *
  * Cats 缺省提供了一些预定义的 typeclass，比如　Functor, Applicative 和 Monad. 以 Functor 为例，它的定义如下：
  *
      @typeclass trait Functor[F[_]] extends Invariant[F] { self =>
        def map[A, B](fa: F[A])(f: A => B): F[B]

        * 接下来几乎所有函数都直接调用了 map，因为这本就是个 Functor
        override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = map(fa)(f)
        ...

        * 除了 map 外，Functor　还提供了 compose 和 composeContravariant 方法来绑定更复杂的组合类型，一个组合类
        * 意味着一个容器套着另外一个容器，例如 List[Either[String, Future[A]]]，那么如果展开的话，我们实际上在处理
        * 一个 _.map(_.map(_.map(f)))... 的循环嵌套操作。为此 Functor 提供了 compose 方法来处理此类参数类型。它
        * 允许参数的形式是 F[G[_]]
        *
        def compose[G[_]: Functor]: Functor[λ[α => F[G[α]]]] =
          new ComposedFunctor[F, G] {
            val F = self
            val G = Functor[G]
          }

        override def composeContravariant[G[_]: Contravariant]: Contravariant[λ[α => F[G[α]]]] =
          new ComposedCovariantContravariant[F, G] {
            val F = self
            val G = Contravariant[G]
          }
      }
  *
  * Functor 允许我们为一个单类型参数的 type constructor，例如 List, Option, Future，或自定义的 MyFunc 指定类型参数
  * 来获得它的实例。接下来以进一步说明：
  * */

/**
  * 一）首先以一个简单的 map 为例：
  * */
object Example_7_Functor_map {
    /** 直接使用 Functor */
    def functor_map() {
        import cats.instances.int._

        /**
          * 要处理的数据。
          **/
        val listOption = List(1, 2, 3)

        /** 1）不需要自己定义Functor，直接引进 */
        import cats.Functor

        /** 2）引进数据类型对应包中的隐式方法。 */
        import cats.instances.list._

        /** 3）绑定 map 函数的处理实体。 */
        val res = Functor[List].map(listOption) { _ + 1 } // List(2, 3, 4)

        assert( List(2, 3, 4) === res)
    }

    /** 也可以用 syntax 方式以隐式类型转换的方式为对象加上 map　*/
    def functor_syntax() {
        /** 预定义函数 */
        val i2d: Int => Double = (x: Int) => (x *2).toDouble
        val d2s: Double => String = (y: Double) => y.toString + "!"

        /** 引进 map syntax */
        import cats.syntax.functor._          // 引进 map 隐式方法
        import cats.instances.function._      // 因为我们希望 map 应用于函数，因此引进 function 隐式方法
        val result1 = (i2d map d2s)(3)        // map 隐式方法通过 macros 生效，因此可能引起 IDE 误报

        import cats.instances.string._
        assert("6.0!" === result1)
        assert("6.0!" === d2s(i2d(3)))         // 等价的使用方式

        /** 也可以用 andThen　等价于 map */
        val result2 = (i2d andThen d2s)(30)
        assert("60.0!" === result2)

        /** 可以直接将 lambda 传递给 map */
        val func = ((x:Int) => (x*2).toDouble).map( x => x.toString + "!")
        assert("6.0!" === func(3))
    }
}

/**
  * 二）处理一个嵌套类型:　F[G[_]]
  * */
object Example_7_Functor_2 {
    def functor_for_container() {
        /** 1）引进 Functor typeclass */
        import cats.Functor

        /**
          * 2）定义要处理的数据。注意 compose 对参数类型的需求是 F[G[_]]，也就是说必须是嵌套类型，不可以是 F[Int]。
          * 详细参考上面的说明
          * */
        val listOption = List(Some(1), None, Some(2))

        /** 3）必须 import 以下数据类型对应包中的隐式方法。*/
        import cats.instances.list._
        import cats.instances.option._

        /** 4）通过 compose 绑定 map 函数的处理实体。*/
        val res = Functor[List].compose[Option].map(listOption) { _ + 1 }  // List(Some(2), None, Some(3))

        import cats.instances.option._
        import cats.instances.int._
        assert(List(Some(2), None, Some(3)) === res)
    }
}

/**
  * 三）更复杂的情况是假设 Functor 本身也希望进行嵌套，也就是说 Functor 的类型参数将变成描述自己的 F[_]，而不是
  * List 之类的具体的高阶类型．
  * */
object Example_7_Functor_3 {
    import cats.Functor

    /** 设计一个 Functor, 这个 Functor 是我们将要嵌套传递的. */
    val listOption = List(Some(1), None, Some(2))

    /** 1）定义一个函数，这个函数的作用是将一个包含数据的容器映射成包含函数的容器，F[A] => F[Unit]
      * 所以它的参数是F[A] 返回值是 F[Unit]．*/
    def NeedsFunctor[F[_]: Functor, A](fa: F[A]): F[Unit] =
    /** 2）定义它的 map 函数体 */
        Functor[F].map(fa)(_ => ())

    /**
      * 3) 将 List(Some(1), None, Some(2)) => List(Some(()), None, Some(()))．
      *
      * 3-1) 首先定义一个 type lambda: ({type λ[A] =  List[Option[A]]})#λ, 作用是将类型 Option 作为缺省，只关注类型 A
      **/
    type ListOption[A] = List[Option[A]]

    /** 3-2) 执行两次连续函数调用，第一次调用是：NeedsFunctor[ListOption, Int](listOption)，传入
      * List(Some(1), None, Some(2))，然后再结果上调用 map(listOptionFunctor)，listOptionFunctor 是一个能够处理 Option 的
      * compose，而 NeedsFunctor 的作用是将数据映射成函数，得到结果 List(Some(()), None, Some(())).
      *
      * 之所以要以这种方式连续调用，是因为我们要将 listOptionFunctor　作用于　Option[A] 中的 A，而不是整个 Option[A]
      * 也就是说我们要实现　Option(fa(a))，否则就成了 fa(Option(a))．也因此我们才需要定义上面的这个 type lambda．
      * 根据 type lambda 提取出 A 的类型 Int，也就是说在第一级函数调用 “NeedsFunctor[ListOption, Int](listOption)”
      * 中，参数 ListOption == F[_] == ListOption[Int]，然后对结果执行函数调用：map(listOptionFunctor) 时 listOptionFunctor
      * 就作用于 Int，而不是 Option[Int] 了.
      * */
    import cats.instances.list._
    import cats.instances.option._
    val listOptionFunctor = Functor[List].compose[Option]
    val mapper = NeedsFunctor[ListOption, Int](listOption)(listOptionFunctor)

    /** 4) 通过 Nested 得到 mapper．将 List(Some(1), None, Some(2)) 重新作用于它，此时的 mapper 只需等待容器内的 () 被赋予
      * 实际有效的函数体．*/
    import cats.data.Nested
    import cats.syntax.functor._
    val nested: Nested[List, Option, Int] = Nested(listOption)

    /** 5) 赋予 mapper 内函数的函数体，完成计算.
      *
      * 必须在编译时加上 scalacOptions += "-Ypartial-unification" 参数. "partial-unification" 源于Scala编译器中
      * 一个臭名昭著的 BUG, 详见 p-70 的介绍。
      *
      * */
    val r = nested.map(_ + 1)
}
