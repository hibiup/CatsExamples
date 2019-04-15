package com.hibiup.cats

package Example_20_Free_Tagless {
    import cats.data._
    import cats.effect.IO
    import cats.free.Free
    import cats.{Monad, ~>}
    import cats.implicits._
    import com.typesafe.scalalogging.Logger
    import org.slf4j.LoggerFactory

    package alg {
        object ADTs {
            /** *************************************
              * 1) ADT
              * */
            trait Result[+A]
            final case class I2F(i: Int) extends Result[BigDecimal]
            final case class F2S(f: BigDecimal) extends Result[String]
            final case class F2B(f: BigDecimal) extends Result[String]

            /** **************************************
              * 2) Lift
              * */
            type FreeResult[A] = Free[Result, A]
        }

        /** ************************************
          * 3) 定义 DSL (代数)
          * */
        trait DSLs {
            import ADTs._

            final private def i2f(i: Int): FreeResult[BigDecimal] = Free.liftF[Result, BigDecimal](I2F(i))
            final private def f2s(f: BigDecimal): FreeResult[String] = Free.liftF[Result, String](F2S(f))
            final private def f2b(f: BigDecimal): FreeResult[String] = Free.liftF[Result, String](F2B(f))

            implicit def isBoolean(s: BigDecimal): InterTypeOps   // 需要实现一个特定抽象类型的运算，用隐式注入得到实例．

            /* Free 组合 */
            final def comp(i: Int): FreeResult[String] = for {
                f <- i2f(i)
                /** 在 for-comprehension 里实现逻辑分支. */
                s <- if (f.canBeBoolean) f2b(f) else f2s(f)    // 使用特定类型的运算（隐式注入）
            } yield s
        }

        // 特定抽象类型的运算接口
        trait InterTypeOps {
            def canBeBoolean: Boolean
        }

        /** *********************************************
          * 4) Free router
          *
          * 业务逻辑解释器
          * */
        trait Compiler[M[_]] {
            import ADTs._
            import scala.language.higherKinds

            def apply[A](action: FreeResult[A])(implicit o:Interpreter[M], monad: Monad[M]): M[A] = action.foldMap(
                new (Result ~> M) {
                // 将一个 Free Monad 映射到一个带有业务运算的函数
                // 需要一个 Monad 实现 ResultT 数据类型的转换
                override def apply[A](fa: Result[A]): M[A] = fa match {
                    case I2F(i) => o.i2f(i).asInstanceOf[M[A]]
                    case F2S(f) => o.f2s(f).asInstanceOf[M[A]]
                    case F2B(f) => o.f2b(f).asInstanceOf[M[A]]
                }
            })
        }

        trait Interpreter[M[_]] {
            def i2f(i: Int): M[BigDecimal]
            def f2s(f: BigDecimal): M[String]
            def f2b(f: BigDecimal): M[String]

            final val compiler = new Compiler[M]{}
        }
    }

    /****************************************************
      * 业务实现
      * */
    package implement {
        import alg._
        import scala.concurrent.{ExecutionContextExecutor, Future}

        object Common {
            /**
              * 到实现的时候才定义返回值和容器的类型
              */
            type Report = Vector[IO[Unit]]
            type λ[α] = WriterT[Future, Report, α]
            type ResultContainer[A] = EitherT[λ, Throwable, A]
        }
        import Common._

        object implicits {
            /*
             * 实现辅助 ResultContainer 运算的 Monad. Cats 和 Scalaz 提供了大部分基本的数据类型的 Monad，因此大部分情况下不需要自己实现．
             * */
            implicit object ResultMonad extends Monad[ResultContainer] {
                implicit val ec: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global
                val logger = Logger(LoggerFactory.getLogger(this.getClass))

                override def flatMap[A, B](fa: ResultContainer[A])(f: A => ResultContainer[B]): ResultContainer[B] = fa flatMap { a => f(a) }

                override def tailRecM[A, B](a: A)(f: A => ResultContainer[Either[A, B]]): ResultContainer[B] = flatMap(f(a)) {
                    case Left(e) => tailRecM(e)(f)
                    case Right(b) => pure(b)
                }

                override def pure[A](x: A): ResultContainer[A] = EitherT {
                    WriterT {
                        Future {
                            (Vector.empty, x.asRight)
                        }
                    }
                }
            }

            /**
              * 实现业务运算
              **/
            implicit object BusinessInterpreter extends Interpreter[ResultContainer] {
                implicit val ec: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global
                val logger = Logger(LoggerFactory.getLogger(this.getClass))

                override def i2f(i: Int): ResultContainer[BigDecimal] = {
                    /**
                      * 根据实现时的需要，实现返回值的装箱.
                      **/
                    EitherT {
                        WriterT {
                            Future {
                                // 返回值（可以拆分出纯业务函数去实现）
                                (
                                        Vector(IO(logger.debug("i2f"))),
                                        if (i >= 0) BigDecimal(i).asRight // 告知顶层的 EitherT 将这个值转载如 right
                                        else new RuntimeException("Input is smaller then 0").asLeft
                                )
                            }
                        }
                    }
                }

                override def f2s(f: BigDecimal): ResultContainer[String] = {
                    EitherT {
                        WriterT {
                            Future {
                                (
                                        Vector(IO(logger.debug("f2s"))),
                                        f.toString.asRight
                                )
                            }
                        }
                    }
                }

                override def f2b(f: BigDecimal): ResultContainer[String] = {
                    EitherT {
                        WriterT {
                            Future {
                                (
                                        Vector(IO(logger.debug("f2b"))),
                                        (if (f < 1) false else true).toString.asRight
                                )
                            }
                        }
                    }
                }
            }

            implicit val compiler = BusinessInterpreter.compiler
        }

        // 实现特定抽象类型的运算
        object DSLs extends DSLs {
            override implicit def isBoolean(s: BigDecimal): InterTypeOps = new InterTypeOps {
                override def canBeBoolean: Boolean = s >= 0 && s <= 1
            }
        }

        package app {
            /** ***************************************
              * 5) 使用
              * */
            object Client extends App {
                import Common._
                import DSLs._
                import implicits._

                import scala.concurrent.Await
                import scala.concurrent.duration.Duration

                // import 某个实现
                implicit val c = implicitly[Compiler[ResultContainer]]

                def run: Unit = {
                    List(-1, 0, 1, 2).foreach { i =>
                        val computation: ResultContainer[_] = c(comp(i)) // Call DSL
                        Await.result(computation.value.run, Duration.Inf) match {
                            case (logs, res) =>
                                logs.foreach(_.unsafeRunSync())
                                res match {
                                    case Left(e: Throwable) => println(e.getMessage)
                                    case Right(a) => println(a)
                                }
                        }
                    }
                }
                run
            }
        }
    }
}
