package com.hibiup.cats

import cats.effect.IO

package Example_21_Effect_IO {
    import scala.concurrent.duration.Duration
    import scala.concurrent.{Await, ExecutionContext, Future}
    import scala.util.{Failure, Success, Try}

    /**
      * IO[A] 为计算提供一个“纯”的计算环境，让计算在返回返回值之前处理“作用”。“作用”是函数“可期待的无副作用，无状态的行为结果”。它不仅
      * 仅通过返回值来体现，“作用”和“返回值”的一大区别是“作用”是 lazy 的，是被期待的结果（结果不仅仅只是返回值）。
      * */

    object io_pure extends App{
        /**
          * IO 仅仅是将计算提升到环境中，直到被 "run" 之前都没有被执行。因此 IO 是纯的.可以被组合, 可预期的返回值可以被传递.
          * 也就是说在实际得到返回值之前, 就可以先对作用进行处理.
          * */
        IO(println("IO"))//.unsafeRunSync()

        /** composite： 传递effect*/
        IO(10).flatMap(i => IO(println(s"Input: $i")))
                .unsafeRunSync()    // run

        /** pure 是 eager 的,下面会立刻被执行. 因此它不能屏蔽参数的副作用.（但是并不代表IO本身不纯） */
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
                    IO/*.pure*/(a)
            }
        }

        println(fib(10000)/*.unsafeRunSync*/)
    }

    /**
      * IO 支持引用透明
      * */
    object io_rt extends App{
        def flatmap(i:Int) = {
            /**
              * 1）Effect 是无状态的，因此下面这段 IO 代码无论执行多少次都会得到相同的输出。而 Future 则不能执行两次。
              * */
            for {
                _ <- IO( i + i).flatMap(a => IO(println(a)))
                _ <- IO( i + i).flatMap(a => IO(println(a)))
            } yield()

            /**
              * 2）透明引用性：因为 IO 是 Lazy 的，因此可以用 task 来取代等号右边的过程，并且在任何时候使用 task 都得到
              * 同样的作用。而 Future 是 eager 的，不支持 rf. （RF 必须是 Laziness的）
              * */
            val add = IO( i + i)
            val task = add.flatMap(a => IO(println(a)))
            for{
                _ <- task
                _ <- task
            } yield ()
        }

        flatmap(2).unsafeRunSync()
    }

    /****************************
      * Future 和 Failure 的处理：
      *
      * Future 是 Eager 的, 因此如果 ExecutionContext 有多余的线程，Future 将立刻在新线程中得到执行，从而产生副作用。
      * 而 IO 则可以将 Future 暂时挂起(因为io的参数是 pass by name 的)直到 unsafeRunSync。如下例所示：
      * */
    object pure_future extends App {
        implicit val ec = concurrent.ExecutionContext.global

        val pureFuture = IO(Future[String]{
            println(s"[${Thread.currentThread.getName}] Future is running....")
            s"[${Thread.currentThread.getName}] Future"
        }).flatMap { f =>      // 传递了 Future
            IO(f.map{s =>      // 传递 Future 的返回值
                s"$s -> [${Thread.currentThread.getName}] map"
            })
        }

        println(pureFuture)  // 得到封存了 Future (纯)的 IO
        // 取出 Future 执行。
        println(s"[${Thread.currentThread.getName}] pureFuture.unsafeRunSync : " + Await.result(pureFuture.unsafeRunSync(), Duration.Inf))   // 以不纯的方式执行 Future
        println(s"[${Thread.currentThread.getName}] pureFuture.unsafeRunSync : " + Await.result(pureFuture.unsafeRunSync(), Duration.Inf))   // 甚至可以多次执行

        /** 一旦从 IO 中获得 Future（f），f 将会恢复 Future 的 eager 本性立刻执行。可以使用 fromFuture 来 defer Future 的
          * 副作用, 避免使用 Await。*/
        val eagerFuture = pureFuture.unsafeRunSync()    // pureFuture 还原出的 Future（f） 会立刻 Eager eval

        val deferredFuture:IO[String] = IO.fromFuture(pureFuture)   // 只得到 pureFuture 的 effect（lazy），避免 eager eval
        // 对 fromFuture 显示调用 unsafeRunSync 来执行 Future
        println(deferredFuture.unsafeRunSync())
        println(deferredFuture.unsafeRunSync())

        /** 直接执行 unsafeRunSync 并不安全, attempt 能够捕获 Failure. attempt.unsafeRunSync fromFuture 可以安全
          * 地得到 Either. 它相当于 try...catch...然后返回 Either[Throwabe, A] */
        IO.fromFuture(IO(Future{
            //s"[${Thread.currentThread.getName}] Future"
            throw new RuntimeException("Boom")
        })).attempt.unsafeRunSync match {   // 得到缺省的 Either
            /** attempt 会将输出封装成 Either[Throwabe, A] 返回 */
            case Right(a) =>
                println(a)   //
            case Left(l) =>
                println(l)   //　得到 RuntimeException
        }

        /** 建议用 IO raiseError 取代 throw 来 defer 副作用 */
        IO.fromFuture(IO(Future{
            //s"[${Thread.currentThread.getName}] Future"
            IO raiseError new RuntimeException("Boom")
        })).attempt.unsafeRunSync match {
            /** attempt 会将输出封装成 Either[Throwabe, A] 返回 */
            case Right(a) =>
                println(a)    // 得到 IO[RuntimeException]
            case Left(l) =>
                println(l)
        }

        /** 错误的例子: 因为类型擦除, 直接尝试从 IO 中获得 Future 的返回类型会失败 !! */
        IO(Future{
            //s"[${Thread.currentThread.getName}] Future"
            throw new RuntimeException("Boom")
        }).attempt.unsafeRunSync.flatMap{
            /** !! 因为类型擦除, case 无法正确判断返回类型. !! */
            case a:Future[Success[String]] =>
                Right(a)
            case f:Future[Failure[Throwable]] =>
                Left(f)
        } match {
            case Right(a) =>
                println(a)
            case Left(l) =>
                println(l)
        }
    }

    /**
      * 上例中 attempt.unsafeRunSync 从 fromFuture 中捕获 Either. async 允许我们显式地采用“异步” 让 Future 执行完成后
      * 通过回调返回 Either。
      *
      * 不管是多线程还是协程 (Future 也不一定运行在新线程中，取决于线程池是否有闲暇线程，否则它可能阻塞在当前线程中。) 的异步，
      * 都需要实现回调。IO.async 的作用就是为某个（异步）任务植入回调，被植入的任务必须具有回调接口，例如 Future 或"观察者模式"
      * 的 onComplete。并且回调不发生在当前线程中, 它处于被植入的线程内.
      *
      * 下面以Future为例:
      *
      * 参考：https://stackoverflow.com/questions/53682686/cats-effect-and-asynchronous-io-specifics
      * */
    object io_async extends App{
        implicit val ec = concurrent.ExecutionContext.global
        /**
          * IO.async 为 Future 提供一个接口。如下例所示：
          * */
        def asyncIO[A](f:Future[A]):IO[A] = IO.async { callback => {
            println(s"[${Thread.currentThread.getName}] Async callback is running")
            /**
              * 利用 onComplete 来呼叫回调函数。（onComplete 本身并不是回调。）
              * */
            f.onComplete {
                case Success(a:String) =>
                    /** 调用回调函数 */
                    callback(Right(s"[${Thread.currentThread.getName}] Async callback : 在 Future 线程中发现 Success - $a".asInstanceOf[A]))
                case Success(x:IO[Throwable]) =>
                    /** 调用回调函数 raiseError 总是返回一个包含一个 Throwable 的 Success */
                    callback(Left(new RuntimeException(s"[${Thread.currentThread.getName}] Async callback : 在 Future 线程中发现 io.raiseError")))
                case Failure(e) =>
                    // 调用回调函数（如果 Future 直接 throw Exception, 则会得到 Failure，否则不会到达这里）
                    callback(Left(new RuntimeException(s"[${Thread.currentThread.getName}] Async callback : 在 Future 线程中发现 Failure - ${e.getMessage}")))
            }
        }}

        /**
          * 处理结果：Option 1)
          *
          * 和前例一样可以通过 unsafeRunSync 或 attempt.unsafeRunSync 在主线程中获得结果。
          *
          * 植入了回调的 IO[Future] 在执行 unsafeRunSync 时，会等待直到回调任务完成，因此不需要显式使用 Await 来等待。
          * */
        val aIO = asyncIO {
            Future{ s"[${Thread.currentThread.getName}] Future: abc" }
        }

        // 可直接获得 Future 的结果无需 Await。
        println(s"[${Thread.currentThread.getName}] aIO.unsafeRunSync : " + aIO.unsafeRunSync())
        println(s"[${Thread.currentThread.getName}] aIO.unsafeRunSync : " + aIO.unsafeRunSync())

        /**
          *　同样 unsafeRunSync 并不安全，attempt.unsafeRunSync 尝试在主线程中获得 async 中定义的 Either，然后进行处理。
          * （如果没有使用 async 定制 Either 则同上例）
          * */
        asyncIO {
            /** 用 IO raiseError 代替 throw 可以避免 Future 返回 Eager　的 Failure, 以Lazy的 Success[IO[Throwable]] 取而代之 */
            Future(
                IO raiseError /*throw*/ new RuntimeException(s"[${Thread.currentThread.getName}] Async callback: Boom...")
            )
            //Future("Good!")
        }.attempt.unsafeRunSync match {    // 得到由 async 定制的 Either
            case Right(r) =>
                println(s"[${Thread.currentThread.getName}] aIO.attempt.unsafeRunSync : " + r)
            case Left(l) =>
                println(s"[${Thread.currentThread.getName}] aIO.attempt.unsafeRunSync : " + l.getMessage)
        }

        /**
          * Option 2）由于我们在 onComplete 中, 已经将异常"通知" 给了回调，因此我们也可以避免在主线程中处理异常,而转而利用
        * 在 Future 线程中 用 unsafeRunAsync 对回调内容进行进行处理.
          * */
        asyncIO {
             Future(IO raiseError /*throw*/ new RuntimeException(s"[${Thread.currentThread.getName}] Async callback: Boom..."))
            //Future("Good!")
        }.unsafeRunAsync {    /** unsafeRunAsync 可以接受 async 返回的数据并进一步处理。（也发生在相同的线程空间。） */
            case Right(r) =>
                println(s"[${Thread.currentThread.getName}] aIO.unsafeRunAsync : " + r)
            case Left(l) =>
                println(s"[${Thread.currentThread.getName}] aIO.unsafeRunAsync : " + l.getMessage)
        }

        // 主线程适当等待 Async 完成。
        Thread.sleep(1000)
    }

    /*************************************
      * IO.suspend(f) 相当于 IO(f.flatten)，并且是 Lazy 的。也就是说它对参数 f 产生有两个作用：
      *
      * 1）推迟对 f 的 eval
      * 2) flat f 的返回值
      *
      * 结合 IO 的 stake safe，以下得到一个安全的单线程无限循环。
      **/
    object io_suspend extends App {
        def fib(n: Int, a: BigDecimal = 0, b: BigDecimal = 1): IO[BigDecimal] = {
            println(s"[Thread-${Thread.currentThread.getName}]")
            IO.suspend {
                if (n > 0)
                    fib(n - 1, b, a + b)
                else
                    IO.pure(a)
            }
        }

        println(fib(100).unsafeRunSync())
    }

    /**
      * io.shift 切换线程(协程)
      * */
    object io_shift extends App{
        import cats.effect.{IO, ContextShift}

        implicit val ec =  scala.concurrent.ExecutionContext.global
        implicit val contextShift = IO.contextShift(ec)

        val task = IO(s"[${Thread.currentThread.getName}] - task")

        /**
          * 在另一线程中执行 task
          *
          * shift 无返回值（Unit）
          * */
        println(IO.shift{
            print(s"[${Thread.currentThread.getName}] *> ")
            contextShift
        }.flatMap(_ => task).unsafeRunSync())

        /** Cats 为宿主定义了 *> 隐式方法代替 flatMap, 因此可以写成(ContextShift是隐式参数): */
        import cats.implicits._
        println((IO.shift *> task).unsafeRunSync())

        /** 或在当前线程中执行 task 然后将结果交由另一个线程继续处理。*/
        println(task.flatMap(a =>
            IO.shift.map(_ => s"[${Thread.currentThread.getName}] <* $a")).unsafeRunSync())

        /** <* 隐式方法代替 flatMap, 不同的是它返回宿主自己的类型 */
        println((task <* IO.shift).unsafeRunSync())
    }

    /**
      * 当 IO 任务遇到 async 时，会生成一个 callback 回调函数交给目标函数（比如 asyncIO），然后 IO 阻塞监听直到 callback
      * 被目标函数调用为止再继续前进。满足异步的唯一条件是 callback 函数被调用,否则 IO 任务会永远处于阻塞状态。可见 async 本
      * 质上只是一个回调机制，可以被用于非多线程环境，比如我们可以将它用于 Try, 将 Try 转成 Either,只要通过 callback 返回
      * 新的值就可以了：
      * */
    object io_async_for_Try extends App {
        import cats.implicits._
        implicit val ec = scala.concurrent.ExecutionContext.global

        def TryAsyncIO(t:Try[String]): IO[String] = IO.async{ callback => {
            println(s"[${Thread.currentThread.getName}] async")

            /** 将 Try 转换成 Either. 然后回调 callback, 否则宿主 IO 会一直被阻塞。*/
            t match {
                case Success(s) =>
                    callback(Right(s"Async callback receive Success"))
                case Failure(f) =>
                    callback(Left(f))
            }
        } }

        print(TryAsyncIO(Try(throw new RuntimeException("Boom!"))).attempt.unsafeRunSync())
    }

    /**
      * 因为 Async IO 必须通过 callback 来解锁阻塞状态，因此如果 callback 不被调用,那么就会永远阻塞下去,因此可以得到一
      * 个阻塞函数: never
      * */
    object io_never extends App{
        def never: IO[Nothing] = IO.async{_ =>
                println("This async doesn't call CALLBACK function, so it will NEVER return!")
        }
        // 下面函数会处于阻塞状态
        // never.unsafeRunSync()

        // println("done") 永远不会被执行到.
        import cats.implicits._
        (never *> IO(println("done"))).unsafeRunSync()
    }

    /**
    * 利用 IO.shift 可以实现协程切换
      * */
    object io_fork extends App {
        import cats.implicits._
        import cats.effect.ContextShift

        def fib(n: Int, a: BigDecimal=0, b: BigDecimal=1)(implicit cs: ContextShift[IO]): IO[BigDecimal] = {
            println(s"[Thread-${Thread.currentThread.getName}]")
            IO.suspend {
                if (n > 0) {
                    val next:IO[BigDecimal] = fib(n - 1, b, a + b)
                    /** 每 4 个循环切换一个协程。 */
                    if (n % 4 == 0)
                        cs.shift *> next    // 等价于：cs.shift.flatMap(_ => next)， next:IO 满足 RT
                    else
                        next
                }
                else
                    IO.pure(a)
            }
        }

        implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
        println(fib(20).unsafeRunSync())
    }

    /**
      * LiftIO: IO[A] => F[A].
      *
      * 将 IO 容器提升到指定的容器 F
      * */
    object lift_IO_to_Future extends App{
        import cats.effect.{LiftIO, IO}
        import scala.concurrent.Future
        import cats.data.EitherT

        implicit val ec = scala.concurrent.ExecutionContext.global

        type FutureEither[A] = EitherT[Future, Throwable, A]

        /** 1) 定义一个隐式 ListIO[F[_]] 实例。含义是：通过这个 LiftIO 获得一个 F */
        implicit def LiftIO2EitherFuture: LiftIO[FutureEither] = new LiftIO[FutureEither] {
                /** 1-1）实现 liftIO 方法：参数是要转变的 IO，返回类型是 F */
                override def liftIO[A](ioa: IO[A]): FutureEither[A] = {
                    /** 1-2) 将结果装入 EitherT[Future].  */
                    EitherT{
                        ioa.attempt.unsafeToFuture()   // 返回 Future[Either[Throwable, A]]
                    }   // Future[Either] => EitherT[Future]
                }
            }

        /** 实现 IO. （这个 IO 可能会失败，但是不用担心，因为 LiftIO2EitherFuture 会将结果装入 FutureEither）*/
        val io = {
            val msg = s"[${Thread.currentThread.getName}] - Normal IO)"
            println(msg)
            IO raiseError new RuntimeException(msg)  //IO(msg)
        }

        /** liftIO 将 IO 提升为 FutureEither */
        val futureEither: FutureEither[String] = implicitly[LiftIO[FutureEither]].liftIO(io)
        Await.result(futureEither.value, Duration.Inf) match {
            case Right(r) => println(s"Message: $r")
            case Left(l) => println(s"Exception: ${l.getMessage}")
        }
    }

    object lift_IO_mix_with_Future extends App{
        import cats.effect.{LiftIO, IO}
        import scala.concurrent.Future
        import cats.data.EitherT
        import cats.implicits._

        import scala.concurrent.ExecutionContext.Implicits.global

        type FutureEither[A] = Future[Either[Throwable, A]]
        implicit def LiftIO2EitherFuture: LiftIO[FutureEither] = new LiftIO[FutureEither] {
            /** 1-1）实现 liftIO 方法：参数是要转变的 IO，返回类型是 F */
            override def liftIO[A](ioa: IO[A]): FutureEither[A] = {
                /** 1-2) 将结果装入 FutureEither.  */
                ioa.attempt.unsafeToFuture()
            }
        }

        // 以下运算都是 Future[Either]
        val service1: FutureEither[Int] = Future(Right(22))
        val service2: FutureEither[Boolean] = Future(Right(false))
        val service3: FutureEither[String] = Future(Left(new Exception("boom!")))

        def program: FutureEither[String] =
            (for {
                n <- EitherT(service1)    //将 Future[Either] 变成 EitherT[Future]
                x <- EitherT(service2)    // x == false
                y <- EitherT(
                    if (x)
                        /** 在 EitherT[Future] 的运算中混入 IO. 通过 LiftIO 将 IO => Future[Either] */
                        implicitly[LiftIO[FutureEither]].liftIO(IO("from io"))
                    else
                        service3)
            } yield y).value

        Await.result(program, Duration.Inf) match {
            case Right(r) => println(s"Result: $r")
            case Left(l) => println(s"Exception: ${l.getMessage}")
        }
    }
}
