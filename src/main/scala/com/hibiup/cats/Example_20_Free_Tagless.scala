package com.hibiup.cats

package Example_20_Free_Tagless {
    import cats.data._
    import cats.effect.IO
    import cats.free.Free
    import cats.{Monad, ~>}
    import cats.implicits._
    import com.typesafe.scalalogging.Logger
    import org.slf4j.LoggerFactory

    package design {
        trait ALGs {    // 代数抽象
        // 使用到的类型
        type InputType
            type InterType
            type OutputType


            /** *************************************
              * 1) ADT
              * */
            trait Result[+A]
            final case class I2F(i: InputType) extends Result[InterType]
            final case class F2S(f: InterType) extends Result[OutputType]
            final case class F2B(f: InterType) extends Result[OutputType]


            /** **************************************
              * 2) Free Lift
              * */
            type FreeResult[A] = Free[Result, A]


            /** ************************************
              * 3) 定义 DSL
              * */
            final private def i2f(i: InputType): FreeResult[InterType] = Free.liftF[Result, InterType](I2F(i))
            final private def f2s(f: InterType): FreeResult[OutputType] = Free.liftF[Result, OutputType](F2S(f))
            final private def f2b(f: InterType): FreeResult[OutputType] = Free.liftF[Result, OutputType](F2B(f))

            implicit def isBoolean(s: InterType): InterTypeOps   // 需要一个 type class 支持特定的条件运算，用隐式注入得到实例．

            /* Free 组合 */
            final def comp(i: InputType): FreeResult[OutputType] = for {
                f <- i2f(i)
                /** 在 for-comprehension 里实现逻辑分支. */
                s <- if (f.canBeBoolean) f2b(f) else f2s(f)      // 使用 type class 支持运算（隐式注入）
            } yield s


            /** *********************************************
              * 4) Free 路由
              *
              * 暂时不理会返回值的容器类型 M，将在实现时考虑
              * */
            trait Compiler[M[_]] {
                import scala.language.higherKinds

                // 需要一个业务逻辑解释器
                def apply[A](action: FreeResult[A])(implicit o:Interpreter[M], monad: Monad[M]): M[A] =
                // 将一个 Free Monad 映射到一个带有业务运算的函数
                // 需要一个 Monad 实现 M(ResultContainer) 数据类型的装箱运算
                    action foldMap new (Result ~> M) {
                        override def apply[A](fa: Result[A]): M[A] = fa match {
                            case I2F(i) => o.i2f(i).asInstanceOf[M[A]]
                            case F2S(f) => o.f2s(f).asInstanceOf[M[A]]
                            case F2B(f) => o.f2b(f).asInstanceOf[M[A]]
                        }
                    }
            }

            /**
              * 业务逻辑解释器
              * */
            trait Interpreter[M[_]] {
                def i2f(i: InputType): M[InterType]
                def f2s(f: InterType): M[OutputType]
                def f2b(f: InterType): M[OutputType]

                final val compiler = new Compiler[M]{}
            }
        }

        // 支持抽象类型的 type class
        trait InterTypeOps {
            def canBeBoolean: Boolean
        }
    }

    /****************************************************
      * 业务实现
      * */
    package implement {
        import design._
        import scala.concurrent.{ExecutionContextExecutor, Future}

        /**
          * 到实现的时候才定义返回值和容器的类型
          */
        object types {
            type Report = Vector[IO[Unit]]
            type λ[α] = WriterT[Future, Report, α]
            type ResultContainer[A] = EitherT[λ, Throwable, A]
        }
        import types._


        /** 实现时才具体化所有用到的数据类型 */
        object ALGs extends ALGs {
            type InputType = Int
            type InterType = BigDecimal
            type OutputType = String

            // 实现隐式 type class, 支持对 InterType(BigDecimal) 的条件运算
            override implicit def isBoolean(s: BigDecimal): InterTypeOps = new InterTypeOps {
                override def canBeBoolean: Boolean = s >= 0 && s <= 1
            }
        }
        import ALGs._


        /** 实现对以上数据类型的业务逻辑 */
        object implicits {
            /*
             * 实现辅助 ResultContainer 运算的 Monad.
             * Cats 和 Scalaz 提供了大部分基本的数据类型的 Monad，因此大部分情况下不需要自己实现．
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
              * 实现业务运算（解释器）
              **/
            implicit object BusinessInterpreter extends Interpreter[ResultContainer] {
                implicit val ec: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global
                val logger = Logger(LoggerFactory.getLogger(this.getClass))

                /**
                  * 根据返回值容器实现对返回值的装箱.
                  **/
                type ContentType[A] = (Report, Either[Throwable, A])

                final private def assemble[A](content: ContentType[A]): ResultContainer[A] = EitherT {
                    WriterT {
                        Future {
                            content
                        }
                    }
                }

                // 实现业务方法
                override def i2f(i: InputType): ResultContainer[InterType] = assemble[InterType] {
                    // 返回值（可以进一步拆分出纯业务函数去实现）
                    (
                            Vector(IO(logger.debug("i2f"))),
                            if (i >= 0) BigDecimal(i).asRight // 告知顶层的 EitherT 将这个值转载如 right
                            else new RuntimeException("Input is smaller then 0").asLeft
                    )
                }

                override def f2s(f: InterType): ResultContainer[OutputType] = assemble[OutputType] {
                    (
                            Vector(IO(logger.debug("f2s"))),
                            f.toString.asRight
                    )
                }

                override def f2b(f: InterType): ResultContainer[OutputType] = assemble[OutputType] {
                    (
                            Vector(IO(logger.debug("f2b"))),
                            (if (f < 1) false else true).toString.asRight
                    )
                }
            }

            implicit val compiler:Compiler[ResultContainer] = BusinessInterpreter.compiler
        }
    }

    /** ***************************************
      * 5) 使用时
      * */
    package app {
        import implement._
        import types._
        import implement.implicits._     // 引进实现

        object Client extends App {
            import scala.concurrent.Await
            import scala.concurrent.duration.Duration

            import ALGs._                                            // 引进 DSL 和业务解释器
            implicit val c = implicitly[Compiler[ResultContainer]]   // 得到注入的 Free 路由

            List(-1, 0, 1, 2).foreach { i =>
                val computation: ResultContainer[_] = c(comp(i))      // Call DSL
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
    }
}
