package com.hibiup.cats

package Example_20_Free_Tagless {
    import cats.data._
    import cats.effect.IO
    import cats.free.Free
    import cats.{Monad, ~>}
    import cats.implicits._
    import com.typesafe.scalalogging.Logger
    import org.slf4j.LoggerFactory

    package pure_free {
        package design {

            trait ALGs { // 代数抽象
                // 使用到的类型
                type InputType
                type InterType
                type OutputType

                /** *************************************
                  * 1) ADT
                  * */
                sealed trait Result[+A]
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

                implicit def isBoolean(s: InterType): InterTypeOps // 需要一个 type class 支持特定的条件运算，用隐式注入得到实例．

                /* Free 组合 */
                final def comp(i: InputType): FreeResult[OutputType] = for {
                    f <- i2f(i)

                    /** 在 for-comprehension 里实现逻辑分支. */
                    s <- if (f.canBeBoolean) f2b(f) else f2s(f) // 使用 type class 支持运算（隐式注入）
                } yield s

                /** *********************************************
                  * 4) Free 路由
                  *
                  * 暂时不理会返回值的容器类型 M，将在实现时考虑
                  * */
                trait Compiler[M[_]] {
                    import scala.language.higherKinds

                    // 需要一个业务逻辑解释器
                    def apply[A](action: FreeResult[A])(implicit o: Interpreter[M], monad: Monad[M]): M[A] =
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
                  **/
                trait Interpreter[M[_]] {
                    def i2f(i: InputType): M[InterType]
                    def f2s(f: InterType): M[OutputType]
                    def f2b(f: InterType): M[OutputType]
                    final val compiler = new Compiler[M] {}
                }
            }

            // 支持抽象类型的 type class
            trait InterTypeOps {
                def canBeBoolean: Boolean
            }
        }


        /** **************************************************
          * 业务实现
          * */
        package implement {
            import design._
            import scala.concurrent.{ExecutionContextExecutor, Future}

            /**
              * 到实现的时候才定义返回值和容器的类型
              */
            object types {
                implicit val ec = scala.concurrent.ExecutionContext.global

                type Report = Vector[IO[Unit]]
                type λ[α] = WriterT[Future, Report, α]
                type ResultContainer[A] = EitherT[λ, Throwable, A]

                /** 对于复杂的返回值容器，可以同时定义一个打包方法，以便于 Interpreter 将结果打包.*/
                type ContentType[A] = (Report, Either[Throwable, A])
                implicit class ResultWrapper[A](content: ContentType[A]) {
                    def assemble: ResultContainer[A] = EitherT {
                        WriterT {
                            Future {
                                content
                            }
                        }
                    }
                }
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
                 * Monad[ResultContainer] 实际上是没必要的, 因为 ResultContainer[A] = EitherT[...] 本身就是 Monad,
                 * Cats 和 Scalaz 提供了大部分基本的数据类型的 Monad，因此大部分情况下不需要自己实现．如果是定制类, 就需要实现
                 * Monad[F] 接口. 以下只是个例子, 删除不影响运行.
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
                    /*type ContentType[A] = (Report, Either[Throwable, A])
                    final private def assemble[A](content: ContentType[A]): ResultContainer[A] = EitherT {
                        WriterT {
                            Future {
                                content
                            }
                        }
                    }*/

                    // 实现业务方法
                    override def i2f(i: InputType): ResultContainer[InterType] = {
                        val threadName = Thread.currentThread.getName
                        // 返回值（可以进一步拆分出纯业务函数去实现）
                        (
                            Vector(IO(logger.debug(s"[$threadName] - i2f"))),
                            if (i >= 0) BigDecimal(i).asRight // 告知顶层的 EitherT 将这个值转载如 right
                            else new RuntimeException("Input is smaller then 0").asLeft
                        ).assemble
                    }

                    override def f2s(f: InterType): ResultContainer[OutputType] = {
                        val threadName = Thread.currentThread.getName
                        (
                            Vector(IO(logger.debug(s"[$threadName] - f2s"))),
                            f.toString.asRight
                        ).assemble
                    }

                    override def f2b(f: InterType): ResultContainer[OutputType] = {
                        val threadName = Thread.currentThread.getName
                        (
                            Vector(IO(logger.debug(s"[$threadName] - f2b"))),
                            (if (f < 1) false else true).toString.asRight
                        ).assemble
                    }
                }

                implicit val compiler: Compiler[ResultContainer] = BusinessInterpreter.compiler
            }
        }


        /** ***************************************
          * 使用时
          * */
        package app {
            import implement._
            import types._
            import implement.implicits._ // 引进实现

            object Client extends App {
                import scala.concurrent.Await
                import scala.concurrent.duration.Duration

                import ALGs._ // 引进 DSL 和业务解释器
                implicit val c = implicitly[Compiler[ResultContainer]] // 得到注入的 Free 路由

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
        }
    }

    package free_tagless {
        package design {
            import cats.implicits._
            import cats.free.Free
            import cats.{Monad, ~>}
            import cats.tagless.{autoFunctorK, finalAlg}
            import simulacrum.typeclass

            trait Alg { // 代数抽象
                // 使用到的类型
                type InputType
                type InterType
                type OutputType

                // InterType 需要一个 type class 支持特定的条件运算（在 comp 内），用隐式注入得到实例．
                implicit def isBoolean(s: InterType): InterTypeOps[InterType]

                /** *************************************
                  * 1) 定义 DSL
                  * */
                @finalAlg
                @autoFunctorK(true)
                trait DSL[F[_]] {
                    def i2f(i: InputType): F[InterType]
                    def f2s(f: InterType): F[OutputType]
                    def f2b(f: InterType): F[OutputType]

                    /** 1-2) Free 组合 */
                    final def comp[F[_]: Monad: DSL](i: InputType): F[OutputType] = for {
                        f <- implicitly[DSL[F]].i2f(i)
                        /** 在 for-comprehension 里实现逻辑分支. */
                        s <- if (f.canBeBoolean) implicitly[DSL[F]].f2b(f) else implicitly[DSL[F]].f2s(f) // 使用 type class 支持运算（隐式注入）
                    } yield s
                }
                // 伴随类由 @finalAlg 自动生成, 如果要消除使用时 “import Result.autoDerive._” 误报, 也可以显示添加。
                /*object DSL {
                    def apply[F[_]](implicit inst: DSL[F]): DSL[F] = inst
                }*/

                /** 2) 支持 Free */
                implicit final def toFree[F[_]]: F ~> Free[F, ?] = λ[F ~> Free[F, ?]](t => Free.liftF(t))
            }

            // 支持 InterType.canBeBoolean (在 comp 内) 的 type class 接口
            @typeclass trait InterTypeOps[A] { self =>
                def canBeBoolean: Boolean
            }
        }


        /** **************************************************
          * 业务实现
          * */
        package implement {
            import design._
            import scala.concurrent.Future

            /**
              * 1) 定义返回值和容器的类型
              */
            object types {
                implicit val ec = scala.concurrent.ExecutionContext.global

                /** 设计返回值容器 */
                type Report = Vector[IO[Unit]]
                type λ[α] = WriterT[Future, Report, α]
                type ResultContainer[A] = EitherT[λ, Throwable, A]

                /** 对于复杂的返回值容器，可以同时定义一个打包方法，以便于 Interpreter 将结果打包.*/
                type ContentType[A] = (Report, Either[Throwable, A])
                implicit class ResultWrapper[A](content: ContentType[A]) {
                    def assemble: ResultContainer[A] = EitherT {
                        WriterT {
                            Future {
                                content
                            }
                        }
                    }
                }
            }

            import types._

            /** 2) 具体化所有用到的数据类型 */
            object Alg extends Alg {
                type InputType = Int
                type InterType = BigDecimal
                type OutputType = String

                // 实现隐式 type class, 支持对 InterType(BigDecimal) 的条件运算
                implicit def isBoolean(s: InterType): InterTypeOps[InterType] = new InterTypeOps[InterType] {
                    override def canBeBoolean: Boolean = s >= 0 && s <= 1
                }
            }

            import Alg._

            /** 3) 实现对以上数据类型的业务逻辑 */
            object implicits {
                val logger = Logger(LoggerFactory.getLogger(this.getClass))

                /*
                 * 3-2) Monad[ResultContainer] 是没必要的,因为 ResultContainer[A] = EitherT[...] 本身就是 Monad,
                 * 如果是定制类,需要实现 Monad[F] 接口. 参见上例.
                 *
                 *    implicit object ResultMonad extends Monad[ResultContainer] {}
                 */

                /** 3-1) 实现业务逻辑 */
                implicit object Interpreter extends DSL[ResultContainer] {
                    def i2f(i: InputType): ResultContainer[InterType] = {
                        val threadName = Thread.currentThread.getName
                        // 返回值（可以进一步拆分出纯业务函数去实现）
                        (
                            Vector(IO(logger.debug(s"[$threadName] - i2f"))),
                            if (i >= 0) BigDecimal(i).asRight // 告知顶层的 EitherT 将这个值转载如 right
                            else new RuntimeException("Input is smaller then 0").asLeft
                        ).assemble // 调用隐式注入的打包方法打包结果
                    }

                    def f2s(f: InterType): ResultContainer[OutputType] = {
                        val threadName = Thread.currentThread.getName
                        (
                            Vector(IO(logger.debug(s"[$threadName] - f2s"))),
                            f.toString.asRight
                        ).assemble
                    }

                    def f2b(f: InterType): ResultContainer[OutputType] = {
                        val threadName = Thread.currentThread.getName
                        (
                            Vector(IO(logger.debug(s"[$threadName] - f2b"))),
                            (if (f < 1) false else true).toString.asRight
                        ).assemble
                    }
                }
            }
        }


        /** ***************************************
          * 使用时
          * */
        package app {
            import cats.arrow.FunctionK
            import implement._
            import types._
            import implicits._ // 引进实现

            object Client extends App {
                import scala.concurrent.Await
                import scala.concurrent.duration.Duration

                import Alg._
                import Interpreter._       // 引进 DSL
                import DSL.autoDerive._

                // Free
                List(-1, 0, 1, 2).foreach { i =>
                    Await.result(comp[Free[ResultContainer, ?]](i).foldMap(FunctionK.id).value.run, Duration.Inf) match {
                        case (logs, res) =>
                            logs.foreach(_.unsafeRunSync())
                            res match {
                                case Left(e: Throwable) => println(e.getMessage)
                                case Right(a) => println(a)
                            }
                    }
                }

                // None Free
                List(-1, 0, 1, 2).foreach { i =>
                    Await.result(comp[ResultContainer](i).value.run, Duration.Inf) match {
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

    package free_non_future_tagless {
        package design {
            import cats.implicits._
            import cats.free.Free
            import cats.{Monad, ~>}
            import cats.tagless.{autoFunctorK, finalAlg}
            import simulacrum.typeclass

            trait Alg { // 代数抽象
            // 使用到的类型
            type InputType
                type InterType
                type OutputType

                // InterType 需要一个 type class 支持特定的条件运算（在 comp 内），用隐式注入得到实例．
                implicit def isBoolean(s: InterType): InterTypeOps[InterType]

                /** *************************************
                  * 1) 定义 DSL
                  * */
                @finalAlg
                @autoFunctorK(true)
                trait DSL[F[_]] {
                    def i2f(i: InputType): F[InterType]
                    def f2s(f: InterType): F[OutputType]
                    def f2b(f: InterType): F[OutputType]

                    /** 1-2) Free 组合 */
                    final def comp[F[_]: Monad: DSL](i: InputType): F[OutputType] = for {
                        f <- implicitly[DSL[F]].i2f(i)
                        /** 在 for-comprehension 里实现逻辑分支. */
                        s <- if (f.canBeBoolean) implicitly[DSL[F]].f2b(f) else implicitly[DSL[F]].f2s(f) // 使用 type class 支持运算（隐式注入）
                    } yield s
                }
                // 伴随类由 @finalAlg 自动生成, 如果要消除使用时 “import Result.autoDerive._” 误报, 也可以显示添加。
                /*object DSL {
                    def apply[F[_]](implicit inst: DSL[F]): DSL[F] = inst
                }*/

                /** 2) 支持 Free */
                implicit final def toFree[F[_]]: F ~> Free[F, ?] = λ[F ~> Free[F, ?]](t => Free.liftF(t))
            }

            // 支持 InterType.canBeBoolean (在 comp 内) 的 type class 接口
            @typeclass trait InterTypeOps[A] { self =>
                def canBeBoolean: Boolean
            }
        }


        /** **************************************************
          * 业务实现
          * */
        package implement {
            import design._

            /**
              * 1) 定义返回值和容器的类型
              */
            object types {
                implicit val ec = scala.concurrent.ExecutionContext.global

                /** 设计返回值容器 */
                type Report = Vector[IO[Unit]]
                type L[α] = Writer[Report, α]
                type ResultContainer[A] = EitherT[L, Throwable, A]

                /** 对于复杂的返回值容器，可以同时定义一个打包方法，以便于 Interpreter 将结果打包.*/
                type ContentType[A] = (Report, Either[Throwable, A])
                implicit class ResultWrapper[A](content: ContentType[A]) {
                    def assemble: ResultContainer[A] = EitherT {
                        Writer(content._1, content._2)
                    }
                }
            }

            import types._

            /** 2) 具体化所有用到的数据类型 */
            object Alg extends Alg {
                type InputType = Int
                type InterType = BigDecimal
                type OutputType = String

                // 实现隐式 type class, 支持对 InterType(BigDecimal) 的条件运算
                implicit def isBoolean(s: InterType): InterTypeOps[InterType] = new InterTypeOps[InterType] {
                    override def canBeBoolean: Boolean = s >= 0 && s <= 1
                }
            }

            import Alg._

            /** 3) 实现对以上数据类型的业务逻辑 */
            object implicits {
                val logger = Logger(LoggerFactory.getLogger(this.getClass))

                /*
                 * 3-2) Monad[ResultContainer] 是没必要的,因为 ResultContainer[A] = EitherT[...] 本身就是 Monad,
                 * 如果是定制类,需要实现 Monad[F] 接口. 参见上例.
                 *
                 *    implicit object ResultMonad extends Monad[ResultContainer] {}
                 */

                /** 3-1) 实现业务逻辑 */
                implicit object Interpreter extends DSL[ResultContainer] {
                    def i2f(i: InputType): ResultContainer[InterType] = {
                        val threadName = Thread.currentThread.getName
                        // 返回值（可以进一步拆分出纯业务函数去实现）
                        (
                            Vector(IO(logger.debug(s"[$threadName] - i2f"))),
                            if (i >= 0) BigDecimal(i).asRight // 告知顶层的 EitherT 将这个值转载如 right
                            else new RuntimeException("Input is smaller then 0").asLeft
                        ).assemble // 调用隐式注入的打包方法打包结果
                    }

                    def f2s(f: InterType): ResultContainer[OutputType] = {
                        val threadName = Thread.currentThread.getName
                        (
                            Vector(IO(logger.debug(s"[$threadName] - f2s"))),
                            f.toString.asRight
                        ).assemble
                    }

                    def f2b(f: InterType): ResultContainer[OutputType] = {
                        val threadName = Thread.currentThread.getName
                        (
                            Vector(IO(logger.debug(s"[$threadName] - f2b"))),
                            (if (f < 1) false else true).toString.asRight
                        ).assemble
                    }
                }
            }
        }


        /** ***************************************
          * 使用时
          * */
        package app {
            import cats.arrow.FunctionK
            import implement._
            import types._
            import implicits._ // 引进实现

            object Client extends App {
                import Alg._
                import Interpreter._       // 引进 DSL
                import DSL.autoDerive._

                // Free
                List(-1, 0, 1, 2).foreach { i =>
                    comp[Free[ResultContainer, ?]](i).foldMap(FunctionK.id).value.run match {
                        case (logs, res) =>
                            logs.foreach(_.unsafeRunSync())
                            res match {
                                case Left(e: Throwable) => println(e.getMessage)
                                case Right(a) => println(a)
                            }
                    }
                }

                // None Free
                List(-1, 0, 1, 2).foreach { i =>
                    comp[ResultContainer](i).value.run match {
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

    package free_option_tagless {
        package design {
            import cats.implicits._
            import cats.free.Free
            import cats.{Monad, ~>}
            import cats.tagless.{autoFunctorK, finalAlg}
            import simulacrum.typeclass

            trait Alg { // 代数抽象
            // 使用到的类型
            type InputType
                type InterType
                type OutputType

                // InterType 需要一个 type class 支持特定的条件运算（在 comp 内），用隐式注入得到实例．
                implicit def isBoolean(s: InterType): InterTypeOps[InterType]

                /** *************************************
                  * 1) 定义 DSL
                  * */
                @finalAlg
                @autoFunctorK(true)
                trait DSL[F[_]] {
                    def i2f(i: InputType): F[InterType]
                    def f2s(f: InterType): F[OutputType]
                    def f2b(f: InterType): F[OutputType]

                    /** 1-2) Free 组合 */
                    final def comp[F[_]: Monad: DSL](i: InputType): F[OutputType] = for {
                        f <- implicitly[DSL[F]].i2f(i)
                        /** 在 for-comprehension 里实现逻辑分支. */
                        s <- if (f.canBeBoolean) implicitly[DSL[F]].f2b(f) else implicitly[DSL[F]].f2s(f) // 使用 type class 支持运算（隐式注入）
                    } yield s
                }
                // 伴随类由 @finalAlg 自动生成, 如果要消除使用时 “import Result.autoDerive._” 误报, 也可以显示添加。
                /*object DSL {
                    def apply[F[_]](implicit inst: DSL[F]): DSL[F] = inst
                }*/

                /** 2) 支持 Free */
                implicit final def toFree[F[_]]: F ~> Free[F, ?] = λ[F ~> Free[F, ?]](t => Free.liftF(t))
            }

            // 支持 InterType.canBeBoolean (在 comp 内) 的 type class 接口
            @typeclass trait InterTypeOps[A] { self =>
                def canBeBoolean: Boolean
            }
        }


        /** **************************************************
          * 业务实现
          * */
        package implement {
            import design._
            import scala.concurrent.Future

            /**
              * 1) 定义返回值和容器的类型
              */
            object types {
                implicit val ec = scala.concurrent.ExecutionContext.global

                /** 设计返回值容器 */
                type Report = Vector[IO[Unit]]
                type L[α] = Writer[Report, α]
                type ResultContainer[A] = OptionT[L, A]

                /** 对于复杂的返回值容器，可以同时定义一个打包方法，以便于 Interpreter 将结果打包.*/
                type ContentType[A] = (Report, Option[A])
                implicit class ResultWrapper[A](content: ContentType[A]) {
                    def assemble: ResultContainer[A] = OptionT {
                        Writer(content._1, content._2)
                    }
                }
            }

            import types._

            /** 2) 具体化所有用到的数据类型 */
            object Alg extends Alg {
                type InputType = Int
                type InterType = BigDecimal
                type OutputType = String

                // 实现隐式 type class, 支持对 InterType(BigDecimal) 的条件运算
                implicit def isBoolean(s: InterType): InterTypeOps[InterType] = new InterTypeOps[InterType] {
                    override def canBeBoolean: Boolean = s >= 0 && s <= 1
                }
            }

            import Alg._

            /** 3) 实现对以上数据类型的业务逻辑 */
            object implicits {
                val logger = Logger(LoggerFactory.getLogger(this.getClass))

                /*
                 * 3-2) Monad[ResultContainer] 是没必要的,因为 ResultContainer[A] = EitherT[...] 本身就是 Monad,
                 * 如果是定制类,需要实现 Monad[F] 接口. 参见上例.
                 *
                 *    implicit object ResultMonad extends Monad[ResultContainer] {}
                 */

                /** 3-1) 实现业务逻辑 */
                implicit object Interpreter extends DSL[ResultContainer] {
                    def i2f(i: InputType): ResultContainer[InterType] = {
                        val threadName = Thread.currentThread.getName
                        // 返回值（可以进一步拆分出纯业务函数去实现）
                        (
                            Vector(IO(logger.debug(s"[$threadName] - i2f"))),
                            if (i >= 0) BigDecimal(i).some // 告知顶层的 EitherT 将这个值转载如 right
                            else None
                        ).assemble // 调用隐式注入的打包方法打包结果
                    }

                    def f2s(f: InterType): ResultContainer[OutputType] = {
                        val threadName = Thread.currentThread.getName
                        (
                            Vector(IO(logger.debug(s"[$threadName] - f2s"))),
                            f.toString.some
                        ).assemble
                    }

                    def f2b(f: InterType): ResultContainer[OutputType] = {
                        val threadName = Thread.currentThread.getName
                        (
                            Vector(IO(logger.debug(s"[$threadName] - f2b"))),
                            (if (f < 1) false else true).toString.some
                        ).assemble
                    }
                }
            }
        }


        /** ***************************************
          * 使用时
          * */
        package app {
            import cats.arrow.FunctionK
            import implement._
            import types._
            import implicits._ // 引进实现

            object Client extends App {
                import scala.concurrent.Await
                import scala.concurrent.duration.Duration

                import Alg._
                import Interpreter._       // 引进 DSL
                import DSL.autoDerive._

                // Free
                List(-1, 0, 1, 2).foreach { i =>
                    comp[Free[ResultContainer, ?]](i).foldMap(FunctionK.id).value.run match {
                        case (logs, res) =>
                            logs.foreach(_.unsafeRunSync())
                            res match {
                                case None => println("Get nothing")
                                case Some(a) => println(a)
                            }
                    }
                }

                // None Free
                List(-1, 0, 1, 2).foreach { i =>
                    comp[ResultContainer](i).value.run match {
                        case (logs, res) =>
                            logs.foreach(_.unsafeRunSync())
                            res match {
                                case None => println("Get nothing")
                                case Some(a) => println(a)
                            }
                    }
                }
            }
        }
    }
}
