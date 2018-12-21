package com.hibiup.cats

import simulacrum.typeclass

/**
  * （p78） - Monad 与 Functor 的不同，或者说 flatMap 和 map 的不同是，map 在序列化运算过程中，它只处理数据本身的复杂性，
  * 它无法保证容器的复杂性也能够被处理，因此它也就不能保证序列化的持续执行．（Monadic 编程就是不断将数据从容器中取出 ->　处理
  * ->　打包 -> 再取出 -> 再处理 -> 再打包 -> ...的过程，因此必须考虑打包的复杂性．）
  *
  * Monad 在 Functor 的基础上需要增加 pure 和 flatMap 方法．
  *
  * trait Monad[F[_]] {
  * def pure[A](value: A): F[A]
  * def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  * }
  *
  *  # pure 方法，在 Haskell 或 Scalaz 中对应为 point 或 return
  *  # flatMap 对应为 bing 或 >>=
  *
  * trait Monad[F[_]] extends FlatMap[F] {
  * def pure[A](value: A): F[A]
  * def point[A]: A => F[A] =  pure
  **
  *def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  *def bind[A, B]: F[A] => (A => F[B]) => F[B] = flatMap
  *}
  *
  * */
object Example_10_Monad {
    /** 既然 pure 方法实际上就是生成 Monad，而且 Cats 提供了预定义的 Monad，因此我们就可以直接生成它：*/
    def cats_monad(): Unit = {
        /**
          * a1) 假设需要得到一个 Option Monad
          * */
        import cats.Monad
        import cats.instances.option._
        val opt = Monad[Option]

        /** a2) 使用 */
        val opt1 = opt.pure(3)
        val opt2 = opt1.flatMap(o => opt.pure(o + 2))
        val opt3 = opt1.flatMap(o => Option(o + 2))  // 和 Option class 没什么区别

        // 检查
        import cats.syntax.eq._
        import cats.instances.option._
        import cats.instances.int._
        assert(opt2 === opt3)


        /**
          * b1) List 可以不需要通过 pure 获得.
          * */
        val list = List(1, 2, 3)
        val res1 = list.flatMap(a => List(a, a * 10))

        /** b1-2) 或通过 Monad 来使用 List flatMap */
        import cats.instances.list._
        val res2 = Monad[List].flatMap(list)(a => List(a, a*10))

        // 检查
        assert(res1 === res2)


        /**
          *  Future 也可以通过 Cats Monad　pure 得到
          * */
        import scala.concurrent.ExecutionContext.Implicits.global

        /** 获得 Future Monad */
        import scala.concurrent.Future
        import cats.instances.future._
        val fm = Monad[Future]

        /** 使用 */
        val f1 = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
        val f2 = fm.flatMap(fm.pure(1))(x => Future(x + 2))  // 和直接使用没区别
        (f1 zip f2).flatMap {
            case a =>Future.successful( a._1 == a._2)
        }.foreach{x =>
            assert(x)}
    }

    /** 上例演示了如何直接使用 Monad 类，Cats 还支持隐式将对象转换成 Monad */
    def cats_monad_syntax(): Unit = {
        import cats.syntax.applicative._ // for pure

        /** Int => Option[Int] */
        import cats.instances.option._ // for Monad
        val opt = 3.pure[Option]

        /** Seq => List[Int] */
        import cats.instances.list._ // for Monad
        val list = 3.pure[List]

        import cats.syntax.eq._
        import cats.instances.int._
        opt.foreach{x => assert(x === list(0))}
    }

    /**
      * 上面演示了通过 Monad trait 来获得 Option．接下来单独讲解一下 Option，Option 实现了 may or may not 运算．
      * 详细解释参考: https://github.com/hibiup/FreeMonad
      *
      * 输入 string number，实现除法运算．
      * */
    def option_monad() = {
        def parseInt(str: String): Option[Int] = scala.util.Try(str.toInt).toOption

        def divide(a: Int, b: Int): Option[Int] = if (b == 0) None else Some(a / b)

        def stringDivideBy(aStr: String, bStr: String): Option[Int] =
            // 将除数和被除数 string => int
            parseInt(aStr).flatMap { aNum =>
                parseInt(bStr).flatMap { bNum =>
                    // 执行除法运算
                    divide(aNum, bNum)
                }
            }

        stringDivideBy("4", "2").foreach(result => println(result))
    }

}
