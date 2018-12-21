package com.hibiup.cats

object Example_11_Monad_Identity {

    /**
      * 通常，Monad 运算都是基于容器的，因此如果我们一般数据就会导致失败，Monad Identity 则允许我们隐式将一般数据转换成 Monad
      *
      * Identity:  幺元，任何数据与幺元结合都返回自身。
      * */
    def monad_id() = {
        import scala.language.higherKinds
        import cats.Monad
        import cats.syntax.functor._ // for map
        import cats.syntax.flatMap._ // for flatMap

        trait operation[A] {
            /** 定义一个函数，只接受 Monad[Int] 类型参数*/
            def apply[F[_]: Monad](a: F[A], b: F[A]): F[A]
        }
        val sumSquare: operation[Int] =  new operation[Int] {
            override def apply[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
                for {
                    x <- a
                    y <- b
                } yield x*x + y*y
        }

        import cats.syntax.eq._
        import cats.syntax.applicative._ // for pure
        import cats.instances.option._ // for option
        import cats.instances.int._
        /** 接纳 Option[Int] */
        assert(Option(25) === sumSquare(3.pure[Option], 4.pure[Option]))

        /** 接纳普通数据类型，隐式转换成 Id Monad */
        import cats.Id
        assert(25 === sumSquare(3 : Id[Int], 4 : Id[Int]))


        /** 测试 String */
        val sumString: operation[String] =  new operation[String] {
            override def apply[F[_] : Monad](a: F[String], b: F[String]): F[String] =
                for {
                    x <- a
                    y <- b
                } yield x + y
        }
        import cats.instances.string._
        assert("Hello, World" === sumString("Hello, " : Id[String], "World" : Id[String]))


        /** 测试 List */
        val sumList =  new operation[List[Int]] {
            override def apply[F[_] : Monad](a: F[List[Int]], b: F[List[Int]]) =
                for {
                    x <- a
                    y <- b
                }yield x ++ y
        }

        import cats.instances.list._
        assert(List(1,2,3,4,5) === sumList(List(1, 2) : Id[List[Int]], List(3,4,5) : Id[List[Int]]))

        /** 测试 Future */
        import scala.concurrent.Future
        import scala.concurrent.ExecutionContext.Implicits.global
        val sumFuture =  new operation[Future[Int]] {
            override def apply[F[_] : Monad](a: F[Future[Int]], b: F[Future[Int]]) =
                for {
                    x <- a
                    y <- b
                }yield x.flatMap(n => y.map(m => n + m))
        }
        sumFuture(Future(1) : Id[Future[Int]], Future(2) : Id[Future[Int]]).foreach(x => assert(3 === x))
    }

}
