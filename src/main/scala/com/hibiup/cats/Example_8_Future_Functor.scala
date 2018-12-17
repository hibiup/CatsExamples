package com.hibiup.cats

import cats.syntax.eq._

import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/** Future 其实就是一个 Functor，它以将异步任务存入队列，并逐个完成计算．它的 map 方法将每个任务的前一次计算的结果应用于后续的计算．*/
object Example_8_Future_Functor {
    def future_map() = {
        import cats.instances.string._

        val future: Future[String] =
            Future(20).

                    /** 下面串行了三个后续任务，每个任务的结果都被传递给下一次运算．因此每个 n 都是前一个的结果． */
                    map(n => n + 1).
                    map(n => n * 2).
                    map(n => n + "!")

        assert("42!" === Await.result(future, 1.second))
    }

    def future_can_only_run_once {
        import cats.instances.int._
        import scala.util.Random

        /** Future 的 map 只运行一次　 */
        val future1 = {
            val r = new Random(0L)
            val x = Future(r.nextInt) // 定义 Future
            for {
                a <- x // 执行 r.nextInt
                b <- x // map _ => _)
            } yield (a, b)
        }
        val result1 = Await.result(future1, 1.second)
        assert(result1._1 === result1._2)

        /** */
        val future2 = {
            val r = new Random(0L)
            for {
                a <- Future(r.nextInt) // 执行 r.nextInt
                b <- Future(r.nextInt) // 再执行一次 r.nextInt
            } yield (a, b)
        }
        val result2 = Await.result(future2, 1.second)
        assert(result2._1 =!= result2._2)
    }
}
