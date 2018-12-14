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

        * Functor　提供了 compose 和 composeContravariant 方法作为实例方法的绑定函数．之所以是 compose 而不是
        * 我们熟悉的 apply 是因为根据 Functor 本身的定义，map 方法必须接受一个高阶类型，并且这个高阶类型允许进一步
        * 展开为一连串的递归调用，也就是说 Functor 必须允许形式为: _.map(_.map(_.map(f)))...的运算,因此它的参数
        * 类型必须是 F[G[_]]
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
  * 接下来以下面例子进一步说明：
  * */
object Example_4_Functor {
    /** 1）不需要自己定义Functor了，可以直接引进 */
    import cats.Functor

    /**
      * 2）定义要处理的数据。注意 compose 对参数类型的需求是 F[G[_]]，也就是说必须是高阶套高阶，不可以是 F[Int]。详细参考
      * 上面的说明
      * */
    val listOption = List(Some(1), None, Some(2))

    /** 3）必须 import 以下数据类型对应包中的隐式方法。*/
    import cats.instances.list._
    import cats.instances.option._

    /** 4）通过 compose 绑定 map 函数的处理实体。*/
    val functor = Functor[List].compose[Option].map(listOption) { _ + 1 }
}
