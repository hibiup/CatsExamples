package com.hibiup.cats

import cats.effect.IO

package Example_21_Effect_IO {

    import scala.concurrent.{ExecutionContext, Future}
    import scala.util.{Failure, Success}

    /**
      * IO[A] 为计算提供一个“纯”的计算环境，让计算在返回返回值之前处理“效果”。“效果”是程序期待的所有外在表现。它不仅
      * 仅通过返回值来体现，效果和返回值的区别是效果是 lazy 的，是被期待的输出（输出不仅仅只是返回值），直到被执行时"效果"
      * 才转换成"返回值"。
      *
      * 因为"效果"并不立刻体现,因此效果一定是 lazy 的
      * */

    object io_pure extends App{
        /**
          * IO 仅仅是将计算提升到环境中，直到被 "run" 之前都没有被执行。因此在 unsafeRunSync 之前它是纯的.
          * 可以被组合, 期待的返回值可以被传递. 也就是说在实际得到返回值之前,就可以先对效果(期待的输出)进行处理而不必担心副
          * 作用.
          * */
        IO(println("IO"))//.unsafeRunSync()

        /** effect 传递(composite)*/
        IO(10).flatMap(i => IO(println(s"Input: $i")))
                .unsafeRunSync()    // run

        /** pure 则是 eager 的,下面会立刻被执行. 因此它不能屏蔽副作用. */
        IO.pure(println("IO.pure"))

        /**
          * IO.apply 和 IO.pure 的区别通过签名可以看出:
          *
          * def pure[A](a: A): IO[A]         // pure 视输入为 pass by value.
          * def apply[A](body: => A): IO[A]  // apply 视输入为 pass by name. pass by name 的参数的执行是被挂起直到
          * 被引用时.
          *
          * 因此以下例子其实并不会因为 readLine 而阻塞,因为返回的 readLn 只是一个带有效果的引用, readLine 被"冻结"在 IO
          * 内部,直到真的被调用的那一刻.
          * */

        def putStrlLn(value: String) = IO(println(value))
        val readLn = IO(scala.io.StdIn.readLine)    //
        println(readLn)   // IO$1310540333   一个纯 IO 实例.

        val pureReadLn = IO.pure(scala.io.StdIn.readLine)  // 会立刻执行 readLine
        println(pureReadLn)   // IO("input")    可以看到 readLine 被当场替换(eval)成了实际输入值
    }

    object io_rt extends App{
        /**
          * IO 和 Future 的不同：
          * */
        def flatmap(i:Int) = {
            /**
              * 1）Effect 的结果不会被记住，因此下面这段代码无论执行多少次都会得到输出。而 Future 则只能得到一次输出。
              * */
            for {
                _ <- IO.pure( i + i).flatMap(a => IO(println(a)))
                _ <- IO.pure( i + i).flatMap(a => IO(println(a)))
            } yield()

            /**
              * 2）透明引用性：因为 IO 是 Lazy 的，因此可以用 task 来取代等号右边的过程，并且在任何时候使用 task 都得到
              * 同样的效果。而 Future 是 eager 的，不支持 rf. （RF 必须是 Laziness的）
              * */
            val add = IO.pure( i + i)
            val task = add.flatMap(a => IO(println(a)))
            for{
                _ <- task
                _ <- task
            } yield ()
        }

        flatmap(2).unsafeRunSync()
    }

    /**
      * IO 是栈安全的，它的 flatMap 是基于 trampoline 的。
      * */
    object stack_safe extends App{
        def fib(n: Int, a: BigDecimal = 0, b: BigDecimal = 1): IO[BigDecimal] = {
            val threadId = Thread.currentThread.getName
            println(s"Thread-[$threadId]")

            IO(a + b).flatMap { b2 =>
                if (n > 0)
                    fib(n - 1, b, b2)
                else
                    IO.pure(a)
            }
        }

        println(fib(100).unsafeRunSync)
    }

    /**
      * async 用来处理一个异步线程的效果, 它将异步效果转换成 Either. 例如以下例子将一个异步的 Future(非纯) 的输出,转换成
      * Either
      * */
    object io_async extends App{
        implicit val ec = concurrent.ExecutionContext.global
        def asyncIO[A](f:Future[A]):IO[A] = IO.async { cb =>
            f.onComplete {
                case Success(a) => {
                    println("Success")
                    cb(Right(a))
                }
                case Failure(e) => {
                    println("Failure")
                    cb(Left(e))
                }
            }
        }

        asyncIO(Future(throw new RuntimeException("Boom...!") /*"IO.async"*/))
                    .flatMap(s => IO(println(s)))
                // 效果会被包裹在 IO 内, 直到 unsafeRunSync
                .unsafeRunSync()
    }


    /*object TestMTL extends App {
        import com.typesafe.config.ConfigFactory
        import cats.Monad
        import cats.effect.LiftIO
        import cats.mtl.ApplicativeAsk
        import cats.mtl._
        import cats.mtl.instances.all._
        import cats.implicits._
        import cats.data.Kleisli

        type Config = String
        type Result = Boolean

        def getConfig: IO[Config] = IO{ConfigFactory.parseString(
            """
              | number.value : 100
            """.stripMargin).getString("number.value")}

        def checkDifference(c:Config):Result = (110-100).abs > Int(c)

        def readerProgram[F[_]: Monad: LiftIO](implicit A: ApplicativeAsk[F,Config]): F[Result] = for {
            config <- A.ask
            result <- IO.pure(checkDifference(config)).to[F]
        } yield result

        val meterializeProgram = readerProgram[Kleisli[IO, Config, ?]]
        println(getConfig.flatMap(meterializeProgram.run).unsafeRunSync())
    }*/
}
