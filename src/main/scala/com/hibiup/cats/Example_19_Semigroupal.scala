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

        /** 因为结合率要求运算可以以任意顺序执行，因此 Semigroupal.product 具有并发执行的能力 */
        val futurePair = Semigroupal[Future].product(
            Future{
                println(s"Thread-${Thread.currentThread.getId}")
                Thread.sleep(1000)
                "Hello"},
            Future{
                println(s"Thread-${Thread.currentThread.getId}")
                123
            })

        import cats.syntax.eq._
        import cats.instances.int._
        import cats.instances.tuple._
        import cats.instances.string._
        assert(("Hello",123) === Await.result(futurePair, 1.second))

        /** Future 与 Semigroupal 结合可以提供很好的数据并发查询能力。 */
        case class Cat( name: String,yearOfBirth: Int, favoriteFoods: List[String] )

        import cats.syntax.apply._
        val futureCat = (
                // 这些数据查询可以并发执行
                Future("Garfield"),
                Future(1978),
                Future(List("Lasagne"))
        ).mapN(Cat.apply)   // product syntax
        assert(Cat("Garfield",1978,List("Lasagne")) == Await.result(futureCat, 1.second))
    }
}
