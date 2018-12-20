package com.hibiup.cats


/**
  * （p78） - Monad 与 Functor 的不同，或者说 flatMap 和 map 的不同是，map 在序列化运算过程中，它只处理数据本身的复杂性，
  * 它无法保证容器的复杂性也能够被处理，因此它也就不能保证序列化的持续执行．（Monadic 编程就是不断将数据从容器中取出 ->　处理
  * ->　打包 -> 再取出 -> 再处理 -> 再打包 -> ...的过程，因此必须考虑打包的复杂性．）
  * */
object Example_10_Monad {
    /**
      * 以 Option Monad 为例：
      *
      * （Option 实现了 may or may not 运算．详细解释参考: https://github.com/hibiup/FreeMonad）
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

    /**
      * 上例的 Future 版本， 尝试用 OptionT 来取得
      * OptionT[F[_], A]
      * */
    def future_monad() {
        import scala.concurrent.ExecutionContext.global
        import scala.concurrent.Future
        import scala.concurrent.Await
        import scala.concurrent.duration._
        import cats.data.OptionT

        implicit val ec = global
        def parseIntFuture(str: String): Future[Option[Int]] = Future{
            println(s"Thread-${Thread.currentThread().getId}")
            scala.util.Try(str.toInt).toOption}
        def divideFuture(a: Int, b: Int): Future[Option[Int]] = Future{
            println(s"Thread-${Thread.currentThread().getId}")
            if (b == 0) None else Some(a / b)}

        def stringDivideByWithFuture(aStr: String, bStr: String) = {
            /** 错误的做法：`.get` 不是一个好的选择，因为如果得到 None 会导致 Exception*/
            val res1 = for {
                (m,n) <- for {
                    aNum <- parseIntFuture(aStr)
                    bNum <- parseIntFuture(bStr)
                } yield (aNum, bNum)
                x <- divideFuture(m.get, n.get)
            } yield x

            /**
              * 正确的做法是用 Cats Option Monad  安全地取出结果．
              *
              * OptionT 是一个 Option monad transformer，它会安全地尝试将结果转换成　OptionT[F[_], A]
              * */
            import cats.instances.future._
            val res2 = (for {
                aNum <- OptionT(parseIntFuture(aStr))
                bNum <- OptionT(parseIntFuture(bStr))
                res <- OptionT(divideFuture(aNum, bNum))
            } yield res).value

            /** 或不使用 Cats 的情况下取得结果． */
            val res3 = (parseIntFuture(aStr) zip parseIntFuture(bStr)).flatMap {
                case (Some(a), Some(b)) => divideFuture(a, b)
                case _ => Future.successful(None)
            }

            res3
        }

        Await.result(stringDivideByWithFuture("6", "2"), 10 seconds).foreach(println)
    }
}
