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
}
