package com.hibiup.cats

object Example_13_ErrorHandling {
    def cats_either() = {
        /**
          * Cats 的 Syntax 包定义了一种方便地将数据隐式打包成希望的 Monoid 的约定 `.asMonoid` 方法。比如：
          * `2.asOption` 等价于 `Option(2)`。对于 Either 类型也是一样的
          * */

        /** asRight 会自动推导出 Right 的数据类型，因此只需要指定左边的类型即可： */
        import cats.syntax.either._
        val r0 = 2.asRight             // Either[Nothing, Int]
        val r1 = 3.asRight[Int]        // Either[Int, Int]
        val r2 = 4.asRight[String]     // Either[String, Int]

        val res = for {
            a <- r0
            b <- r1
            c <- r2
        } yield a + b + c
        println(res)

        import cats.syntax.eq._
        import cats.instances.int._
        res.map(x => assert(9 === x))

        /** 与 asRight 相反 asLeft 会自动推导出 Left 的数据类型，因此只需要指定右边的类型即可： */
        val e = "Error".asLeft[Int]   // Either[String, Int]
        println(e)
        assert( 0 === e.getOrElse(0))

        /** ensure 方法可以用于封装简单的业务条件，的第一个参数是如果不满足的输出结果，第二个参数是判断处理函数 */
        val e1 = (-1).asRight[String].ensure("Must be greater than 0")(_ > 0)
        println(e1)
        assert(e1.isLeft)

        /** leftMap 方法能够用于故障回复 */
        def recover(t:Throwable){println(t.getMessage)}
        new RuntimeException("Boom..!!").asLeft[Int].leftMap(recover)

        /** bimap 可以双向处理 */
        val e2 = (-3).asRight[Throwable].ensure(new RuntimeException(s"Negative number!")){_ > 0}
            .bimap(t => t, _ * 2) /** 返回 ensure 的结果作为 Left */
        assert(e2.isLeft || e2.getOrElse(None).isInstanceOf[RuntimeException])

        (3).asRight[Throwable].ensure(new RuntimeException(s"Negative number!")){_ > 0}
            .bimap(recover, _ * 2)  /** 正确输出 Right(6) */
            .map(x => assert(x === 6))
    }

    /**
      * Cats 设计了一个 MonadError Monad可以作为 Error handler。它有两个最重要的方法：
      *
      *  * handleError 用于接管错误处理
      *  * raiseError 用于抛出错误
      *
      * */
    def monad_error(): Unit = {
        import cats.MonadError
        import cats.instances.either._

        /** 1）定义一个 Either，左边为错误类型，右边待定 */
        type ErrorOr[A] = Either[String, A]    // type lambda

        /** 2）定义一个 MonadError, 接受这个错误类型，并指明错误类型  */
        val monadError = MonadError[ErrorOr, String]

        /** 3）通过这个 MonadError 的 pure 方法可以自动推导出右边的类型，并返回 Right */
        val success = monadError.pure(200)
        println(success)
        assert(success.isRight)

        /** 4）raiseError 返回 Left */
        val failure = monadError.raiseError("Badness")
        println(failure)
        assert(failure.isLeft)

        /** 5) 定义 handleError 来接管错误处理 */
        val res = monadError.handleError(failure) {
            case "Badness" => monadError.pure("You bad!")
            case _ => monadError.raiseError("Other error")
        }
        res.foreach(println)

        /** 6) Monad Error 也支持 ensure 方法.接受三个参数：
          *
          * 第一个是要测试的 Either 数据
          * 第二个是如果错误返回的数据（类型为Either左边的类型）
          * 第三个是测试函数
          * */
        val res1 = monadError.ensure(success)("Negative number!")(_ > 0)
        res1.map(x => assert(x == 200))


        /***********************
          * 和其他 Monad 一样，Cats 支持 MonadError syntax 语法。
          *
          * 引进 applicative 是因为 MonadError 实际上是继承自 ApplicativeError，所以 syntax 来自 applicative 包
          */
        import cats.syntax.applicative._
        val success1 = 200.pure[ErrorOr]
        assert(success1 == success)

        import cats.syntax.monadError._
        assert(Right(200) == success1.ensure("Negative number!")(_ > 0))

        import cats.syntax.applicativeError._
        val failure1 = "Badness".raiseError[ErrorOr, Int]
        assert(failure == failure1)

    }
}
