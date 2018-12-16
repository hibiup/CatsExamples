package com.hibiup.cats

/**
  * Functor syntax
  *
  * Cats 缺省提供了一些预定义的　typeclass，比如　Functor, Applicative 和 Monad. 以 Functor 为例，它的定义如下：
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
  * 接下来以进一步说明：
  * */

/**
  * 一）首先以一个简单的map 为例：
  * */
object Example_4_Functor_1 {
    /** 1）不需要自己定义Functor了，可以直接引进 */
    import cats.Functor

    /**
      * 2）定义要处理的数据。
      * */
    val listOption = List(1, 2, 3)

    /** 3）必须 import 数据类型对应包中的隐式方法。*/
    import cats.instances.list._

    /** 4）绑定 map 函数的处理实体。*/
    val functor = Functor[List].map(listOption) { _ + 1 }  // List(2, 3, 4)
}

/**
  * 二）处理一个嵌套类型:　F[G[_]]
  * */
object Example_4_Functor_2 {
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
    val functor = Functor[List].compose[Option].map(listOption) { _ + 1 }  // List(Some(2), None, Some(3))
}

/**
  * 三）更复杂的情况是假设 Functor 本身也希望进行嵌套，也就是说 Functor 的类型参数将变成描述自己的 F[_]，而不是
  * List 之类的具体的高阶类型．
  * */
object Example_4_Functor_3 {
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

    /** 5) 赋予 mapper 内函数的函数体，完成计算 */
    val r = nested.map(_ + 1)
}
