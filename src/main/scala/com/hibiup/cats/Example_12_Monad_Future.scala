package com.hibiup.cats

object Example_12_Monad_Future {

    /**
      * 尝试在 Future 中通过 OptionT[F[_], A] 使用 Option
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
