package com.hibiup.cats

import cats.syntax.eq._
import cats.instances.int._
import cats.instances.string._

/**
  * Cats 实现了 Writer 和 Reader Monad 作为对外部 IO 操作 的基础。
  *
  * Writer 实际上就是一个缓冲区，它竟可能地将写操作推迟到计算的边缘。
  * */
object Example_15_Monad_Writer {
    def writer_example(): Unit = {
        /***************************************
          *  Writer 含有两个类型参数。分别对应它的两个参数，可以将 Writer 的两个参数看成一个 pair。因为 Writer 的典型应用场景是
          *  用于记录日志，因此第一个参数被称为 log．而第二个参数可以用来存放最终的结果或参数，称为 result。但是二者都不是必须的，
          *  可以为空。
          *
          *  例如下面的例子，将一条数据存入一个 Writer，结果为空。
          * */
        import cats.data.Writer
        val w = Writer("It's a good day...", ())   // 表示写入一条日志，但是不期待返回任何内容
        println(w)
        /** 需要注意的是, Writer 只是个满足 Monad 的计算框架, 它本身并不意味着一定会产生 IO */

        /** log 也可以是一个 Sequence */
        import cats.instances.vector._
        val w1 = Writer(Vector("msg1", "msg2", "msg3"), 123)
        // 当然，结果类型也支持 Sequence:
        val _w1 = Writer(123, Vector("msg1", "msg2", "msg3"))
        assert(w1 === _w1.swap)  // swap 函数交换 log 和 result

        /** 获取存放其中的数据 */
        val wRes: Int = w1.value                 // 获得 result
        val wLog: Vector[String] = w1.written    // 获得 log
        val (log, result) = w1.run               // 同时获得
        assert(wRes === result)
        assert(wLog === log)

        /** Syntax 用法　*/
        import cats.syntax.writer._
        val w2 = 123.writer(Vector("msg1", "msg2", "msg3"))
        assert(w2 === w1)

        /****************************************
          * Writer 的两个参数都不是必须的，比如没有第二个参数（结果类型为 Unit）:
          * */
        val w3 = Vector("msg1", "msg2", "msg3").tell   // tell 函数生成一个不带返回值的 Writer
        val w3Log = w3.written
        assert(w3Log === Vector("msg1", "msg2", "msg3"))

        import cats.instances.unit._
        assert(() === w3.value)

        /** 如果只有结果类型，没有日志数据（也就意味着日志为幺元）:　*/
        import cats.syntax.applicative._
        type Logged[A] = Writer[Vector[String], A]
        val w4 = 123.pure[Logged]                      // pure 函数为返回值生成一个包含 identity 的 Writer.

        val w4Res = w4.value
        assert(w4Res === 123)
        assert(w4.written.size === 0)
    }

    def writer_monad_computation(): Unit = {
        import cats.data.Writer
        import cats.syntax.writer._
        import cats.syntax.applicative._
        import cats.instances.vector._

        /***************************************
          * Writer 缺省的 flatMap 函数被设计成对 log 具有 append 运算的效果，适合用于对日志的追加运算．
          *
          * p-111:
          * The log in a Writer is preserved when we map or flatMap over it. flatMap appends the logs from the
          * source Writer and the result of the user’s sequencing function.
          */
        type Logged[A] = Writer[Vector[String], A]
        val w1 = for {
            a <- 10.pure[Logged]               // 生成 Writer(Vector(), 10)
            _ <- Vector("a", "b", "c").tell    // 对 Vector() append Vector("a", "b", "c") => Vector("a", "b", "c")，返回 ()
            b <- 32.writer(Vector("x", "y", "z"))   // append 出 Vector("a", "b", "c", "x", "y", "z")，并返回 32
        } yield a + b                          // 得到 Writer(Vector("a", "b", "c", "x", "y", "z"), 10 + 32)

        import cats.instances.vector._
        import cats.instances.string._
        import cats.instances.int._
        import cats.instances.tuple._
        assert((Vector("a", "b", "c", "x", "y", "z"),42) === w1.run)

        /** 以上等价于：*/
        val _w1 = 10.pure[Logged].flatMap{
            Writer(Vector("a", "b", "c"), _).flatMap { x =>
                Writer(Vector("x", "y", "z"), 32).map{ y =>
                    x + y
                }
            }
        }
        assert(w1 === _w1)

        /**
          * map 函数接受一个函数参数,处理 log 和 result．
          *
          * mapWritten　作用于 log
          * */
        val w2 = w1.mapWritten(_.map(_.toUpperCase))
        assert((Vector("A", "B", "C", "X", "Y", "Z"),42) === w2.run)

        /** map 作用于 result */
        val w22 = w1.map(_ * 100)
        assert((Vector("a", "b", "c", "x", "y", "z"),4200) === w22.run)

        /** bimap 同时作用于 log 和 result */
        val w3 =w1.bimap(
            log => log.map(_.toUpperCase),
            res => res * 100
        )
        assert((Vector("A", "B", "C", "X", "Y", "Z"),4200) === w3.run)

        /** reset 返回一个清空 log 的 Writer：*/
        val w4 = w1.reset    // 注意，reset 并不清空当前 Writer，而是生成一个新的 Writer
        assert(w4.run === (Vector(), 42))
    }

    /**
      * 对于一个自定义的类，只需要这个类满足 Monoid 就可以被使用。
      * */
    def writer_for_customized_type(): Unit = {
        /** 1) 自定义类型 */
        case class Cat(name: String, favoriteFood: String)

        /**
          * 2) 实现 CatWriter appender，也就是实现这个 Cat 的 Monoid.
          *
          *    * 它的 combine 方法就是 Monoid 上的 append
          *    * 它的 empty 就是 pure
          *
          *    详见 Example_6_Monoid
          * */
        import cats.Monoid
        implicit object catSemigroup extends Monoid[Cat] {
            override def combine(x: Cat, y: Cat): Cat = Cat(x.name + "&" + y.name, x.favoriteFood + "&" +y.favoriteFood)
            override def empty: Cat = Cat("","")
        }

        /** 3) 自定义 Writer */
        import cats.data.Writer
        type CatWriter[A] = Writer[Cat, A]

        /** 4）可以使用这个 Cat 的 flatMap 进行运算了。*/
        import cats.syntax.applicative._
        val w1 = 10.pure[CatWriter].flatMap{
            Writer(Cat("Mimi","Fish"), _).flatMap { x =>
                Writer(Cat("Miaomiao","Mice"), 32).map{ y =>
                    x + y
                }
            }
        }

        assert((Cat("&Mimi&Miaomiao","&Fish&Mice"),42) == w1.run)
    }

    /**
      * Writer 是线程安全的，因此可以用于多线程环境
      * */
    def writer_in_multiple_thread(): Unit = {
        import cats.data.Writer
        type Logged[A] = Writer[Vector[String], A]

        /** 1) 定义一个运行缓慢的函数，它会休眠 100 毫秒 */
        def slowly[A](do_something: => A) = try do_something finally Thread.sleep(100)

        /** 2) 工作函数，将函数体交由前面定义的函数执行，强制让它变慢 */
        import cats.syntax.writer._
        import cats.syntax.applicative._
        def factorial(n: Int): Logged[Int] = {
            import cats.instances.vector._
            for{
                ans <- slowly(
                    if ( n == 0) 1.pure[Logged]         // pure 生成 identified Logged result = 0
                    else factorial(n - 1).map(_ * n) )  // 修改最后的 result
                _ <- Vector(s"fact $n $ans").tell       // tell append log
            } yield ans
        }

        /** 3) 定义多个线程，启动多个函数实例：*/
        import scala.concurrent.Future
        import scala.concurrent.ExecutionContext.global
        implicit val ec = global
        val futures = Future.sequence(Vector(
            Future(factorial(3).run),
            Future(factorial(5).run)
        ))

        import scala.concurrent.Await
        import scala.concurrent.duration._
        val Vector((logA, ansA), (logB, ansB)) = Await.result(futures, 5 seconds)
        assert(ansA === 6)
        assert(ansB === 120)

        println(logA)
        println(logB)
    }

    /** */
    def writer_for_greeting(): Unit = {
        type Prompt = String
        type Reply = String
        type Message = String
        type Interaction[A] = Map[Prompt, Reply] => (List[Message], A)

        import cats.data.Writer
        type Logged[A] = Writer[Vector[String], A]

        import cats.syntax.applicative._
        import cats.instances.vector._
        import java.util.Scanner
        def ask(prompt: String) = {
            println(prompt)
            val scanner = new Scanner(System.in)
            scanner.nextLine().pure[Logged]
        }
        def tell(msg: String) = msg.pure[Logged]

        val greeting = for {
            first <- ask("What's your first name?")
            last <- ask("What's your last name?")
        } yield tell(s"Hello $first $last")

        println(greeting)
    }
}
