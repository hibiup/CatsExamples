package com.hibiup.cats


import scala.concurrent.Future

object Example_18_Transformative {
    /**
      * “Monad 就像一块大卷饼” 这句话的意思是：在运算的过程中，因为运算环境的复杂性，导致我们在很多时候不能直接处理数据。而需要将运算上下文
      * 也打包（作为容器）考虑进来。比如我们需要从数据库中查询一个用户 User, 但是考虑到 User 很可能不存在，因此需要将数据 up lift 成为
      * Option[User]，再考虑到可能存在多个用户，因此需要再次 up lift 为 List[Option[User]]，如果再考虑运算环境可能导致的故障就会得到
      * Either[Throwable, List[Option[Int]]] 。最终我们得到一个层层包裹着的数据。面对这个卷饼，我们希望在执行 flatMap 和 map 时也
      * 能够穿透层层的外围，到达最内层，就如同外围的复合容器和一个单层容器一样没有区别。为此我们希望 Monad 也可以具有这样的层级关系，也就是
      * 可以通过将一个 Monad 包裹在另一个 Monad 之中得到一个复合的 Monad 来应对包裹在复合容器中的数据。为此 Cats 提供了一系列 Monad
      * Transformer 来实现这种转换（p132 ~ p133）：
      *
      * • cats.data.OptionT for Option
      * • cats.data.EitherT for Either
      * • cats.data.ReaderT for Reader
      * • cats.data.WriterT for Writer
      * • cats.data.StateT for State
      * • cats.data.IdT for the Id monad.
      *
      * 以 OptionT Transformer 为例，例如我们有以下数据类型：
      *
      *   OptionT[List, A]
      *
      * OptionT 与简单容器 Option 不同的是，OptionT 不仅仅将类型 A 升级成 Option[A], 它还将它打包入第一个参数类型，得到复合类性：
      * List[Option[A]]。由此来避免了对数据的层层手工打包。（Transformer 本身也是 Monad），
      *
      * */
    def transformative_example(): Unit = {
        import cats.data.OptionT
        /**
          * 在运算中，我们可以利用 type lambda 将 transformer 定义成一个新的 type:
          *
              type ListOption[A] = OptionT[List, A]
          *
          * type lambda 会将 List 作为 ListOption 的参数传递给 OptionT，于是在编译时展开为：
          *
              ListOption(a) => OptionT[List, _](a)
          *
          */
        type ListOption[A] = OptionT[List, A]

        import cats.instances.list._
        import cats.syntax.applicative._
        val result1: ListOption[Int] = 32.pure[ListOption]  // 没有 ListOption(a) 构造方法，但可以利用 Cats 提供的 syntax (p132)

        // 检验
        import cats.instances.int._
        import cats.instances.option._
        assert(result1 === OptionT[List, Int](List(Some(32))))

        val result2 = OptionT(List(Option(32)))
        assert(result1 === result2)

        /** 这样一个具有List和Option复合类型的新类型 ListOption，也就是 OptionT[List, _] 在进行 flatMap 和 map 运算时可以忽略容器
          * 的复杂性： List(Option(a+b) */
        assert(result1.flatMap { x =>
            result2.map { y =>
                x + y
            }
        } === OptionT[List, Int](List(Option(64))))
    }

    /**
      * 如上面的例子看到的，type lambda 透明地将第一个类型变量传递给目标对象，的这种传递本身并不是 transformer 的定义，但是transformer
      * 却可以从中受益，使得定义看上去更简洁。
      * */
    def either_option_transformer(): Unit = {
        import cats.implicits._  // 隐式引进所有 Cats syntax, data 和 instance。在应用中最好定义在 package object 中

        /** 1) 利用 type lambda 得到一个具有隐含第一参数的 type */
        type ErrorOr[A] = Either[String, A]

        /** 2) 将新的 type 用于 Transformer */
        import cats.data.OptionT
        type ErrorOrOption[A] = OptionT[ErrorOr, A]

        /** 3) ErrorOr 能够将数据进一步传递给 Either */
        val a = 10.pure[ErrorOrOption]
        assert(a === OptionT[ErrorOr, Int](Right(Some(10))))
    }

    def future_either_transformer(): Unit = {
        import scala.concurrent.Future
        import cats.data.{EitherT, OptionT}

        /** 1) EitherT 将 Either[Throwable, A] 装入 Future. EitherT 将作用在后两个参数上 */
        type FutureEither[A] = EitherT[Future, Throwable, A]

        /** 2）再将 FutureEither[A] 作为 Option[A] 的容器类型. 得到的最终“大饼”类型为：
          *
          *      Future[Either[Throwable,[Option[A]]]]
          *
          *   注意包装的顺序是从外到内。
          * */
        type FutureEitherOption[A] = OptionT[FutureEither, A]

        import scala.concurrent.ExecutionContext.Implicits.global
        import cats.implicits._

        /** 3) OptionT(EitherT(Future(...))) 将返回 Future(Success(Right(Option(10))) */
        val res = 10.pure[FutureEitherOption]
        import scala.concurrent.Await
        import scala.concurrent.duration._

        /** 4) value 方法用于解包 monad 得到每部的值。 */
        val stackF = res.value.value
        val f = Await.result(stackF, 10 seconds)
        assert(f.isRight)
        assert(Some(10) == f.getOrElse(None))

        /** 4-1）也可以忽略容器的层级，直接处理最终值，但是要注意在这个例子中，assert 将发生在另外一个线程内，因此测试线程将捕捉不到错误。 */
        val f2 = (for {
                a <- 30.pure[FutureEitherOption]
                b <- 10.pure[FutureEitherOption]
            } yield a / b).value.value  // value.value 解包直到 Future，然后用 recover 捕获异常
                    /** recover 是 Future 的 catch */
                    .recover{ case e:Throwable => Left(e)
            } // 返回 Future

        /** 5）Either 的双向处理。*/
        f2.map{
            case Left(t) => println(t.getMessage)
            case Right(i) => {
                println(i)
                assert(Option(3) === i)
            }
        }

        Await.result(f2,10 seconds)
    }
}
