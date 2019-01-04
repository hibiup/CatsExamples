package com.hibiup.cats

//import cats.implicits._

object Example_19_Semigroupal {
    def semigroupal_examples(): Unit = {
        import cats.Semigroupal

        /** product：“乘积”，表达的是在一个 Semigroup 中任意两个成员的结合率。
          *
          * 以 Option 例，任意两个成员的乘积将产生一个 Tuple2 */
        import cats.syntax.eq._
        import cats.instances.option._
        import cats.instances.int._
        import cats.instances.tuple._
        import cats.instances.string._
        import cats.instances.boolean._

        assert(Option((123, "abc")) === Semigroupal[Option].product(Some(123), Some("abc")))
        assert(None == Semigroupal[Option].product(Some(123), None)) // 任何 Option 和 None 的乘积都是 None

        /** tupleX 函数生成最多 22 个参数的乘积 */
        val opt1 =Semigroupal.tuple3(Option(1), Option("abc"), Option(true))
        assert(Option((1,"abc",true)) === opt1 )
        assert(None == Semigroupal.tuple3(Option(1), None, Option("abc")))  // 同样存在 None 会得到 None

        /** mapN 函数可以定义乘积算法 */
        assert(Option(6) === Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _))

        /** 可以直接使用 syntax.apply._ 包提供的隐式来实现以上函数， */
        import cats.syntax.apply._
        val opt2 = (Option(1), Option("abc"), Option(true)).tupled
        assert(opt1 === opt2)

        /** 一个 Tuple 可以通过 mapN 应用于某个算法 */
        case class Cat(name: String, born: Int, color: String)

        val cat1 = ( Option("Garfield"),Option(1978),Option("Orange & black")).mapN(Cat.apply)  // eta conversion
        println(cat1)
        assert(Option(Cat("Garfield", 1978, "Orange & black")) == cat1)

        assert(Option(6) === (Option(1), Option(2), Option(3)).mapN(_ + _ + _))
    }

    def semigroupal_future_executes_paramelly() {
        import cats.Semigroupal
        import cats.instances.future._ // for Semigroupal
        import scala.concurrent._
        import scala.concurrent.duration._
        import scala.concurrent.ExecutionContext.Implicits.global
        import scala.language.higherKinds

        /** 1) 因为结合率要求运算可以以任意顺序执行，因此 Semigroupal.product 具有并发执行的能力 */
        val futurePair = Semigroupal[Future].product(
            Future {
                println(s"Thread-${Thread.currentThread.getId}")
                Thread.sleep(1000)
                "Hello"
            },
            Future {
                println(s"Thread-${Thread.currentThread.getId}")
                123
            })
        import cats.syntax.eq._
        import cats.instances.int._
        import cats.instances.tuple._
        import cats.instances.string._
        assert(("Hello", 123) === Await.result(futurePair, 1.second))

        /** 2) 等价于 zip： */
        val a = Future {
            println(s"Thread-${Thread.currentThread.getId}")
            Thread.sleep(1000)
            "Hello"
        }
        val b = Future {
            println(s"Thread-${Thread.currentThread.getId}")
            123
        }
        val c = a zip b // zip 是并行的
        assert(("Hello", 123) === Await.result(c, 2.second))


        /** 3) Future 与 Semigroupal 结合可以提供很好的数据并发查询能力,也避免了 zip 必须数据对齐的限制。 */
        case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

        import cats.syntax.apply._
        val futureCat = (
                // 这些数据查询可以并发执行
                Future("Garfield"),
                Future(1978),
                Future(List("Lasagne"))
        ).mapN(Cat.apply) // product syntax
        println("====")
        assert(Cat("Garfield", 1978, List("Lasagne")) == Await.result(futureCat, 1.second))


        /** 4) product 的工作方式其实等价于 flatMap */
        val a1 =Future {
            println(s"Thread-${Thread.currentThread.getId}")
            Thread.sleep(1000)
            "Hello"
        }
        val b1 = Future {
            println(s"Thread-${Thread.currentThread.getId}")
            123
        }
        val forFuture1 = for {
            x <- a1
            y <- b1
        } yield (x, y)
        assert(("Hello", 123) === Await.result(forFuture1, 2.second))

        /** 4-1) 但是不等价于以下, 以下两条 <- 指令之间有明显停顿，因为 Future 在被定义的时候就开始执行, 因此上面在执行到
          *    for comprehansion 时 两个 Future 都已经处在执行状态, 而下面的 Future 在循环体内被定义, 因此是顺序执行 flatMap,
          *    并顺序生成 Future.  注意体会两者的区别
          *
          *    同时, product 函数也一样, 我们先定义了 Future 参数, 因此当 product 函数被调用的时候, Future 实际上已经处于执行状态
          *    这就是为什么它看上去具有并行的能力而 for-comprehansion 循环体本身没有（除非参数定义在循环体外）。
          *    */
        val forFuture2 = for {
            x <- Future {
                println(s"Thread-${Thread.currentThread.getId}")
                Thread.sleep(1000)
                "Hello"
            } // 有明显停顿后才执行下一条指令
            y <- Future {
                println(s"Thread-${Thread.currentThread.getId}")
                123
            }
        } yield (x, y)
        assert(("Hello", 123) === Await.result(forFuture2, 2.second))
    }

    def Semigroupal_either() {
        import cats.Semigroupal

        /** product 作用于 Either 时，和 flatMap 一样在遇到第一个失败后会终止后面的执行.这是因为 Cats 的 Monad 继承了
          * Semigroup(也就是说 Cats Monad 也很可能是一个 Monoid, 比如 Option 既是一个 Monoid 也是 Monad ),因此它们
          * 和 Future 等 Monad 的行为是一致的.  */
        import cats.instances.either._
        type ErrorOr[A] = Either[Vector[String], A]
        val f = Semigroupal[ErrorOr].product(
            Left(Vector("Error 1")),  // 第一个失败
            Left(Vector("Error 2"))   // 第二个失败(不会被执行)
        )
        println(f)
    }
}
