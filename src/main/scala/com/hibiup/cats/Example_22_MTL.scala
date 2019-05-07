package com.hibiup.cats

package Example_22_MTL {
    /**
      * https://www.signifytechnology.com/blog/2018/10/a-comprehensive-introduction-to-cats-mtl-by-luka-jacobowitz
      *
      * 'MTL'是一个用于 monad 组合的变换器的库，可以使得 monad 变换器嵌套变得更容易。它起源于Haskell，但是很久以前就已经被引入了Scala。
      * 然而，因为一堆不同的Scala怪癖汇集在一起使得它在很长的时间里它几乎无法使用​。也因此，很多人都觉得mtl是可怕的，太抽象的或太复杂了。在这
      * 篇博文中，我将尽力反驳这些观点并展示 Cats-mtl 的简洁和优雅。读完这篇文章后，我希望你们一致认为，只要你需要将一个以上monad变换器互相
      * 嵌套就应该喜欢'mtl'。
      *
      * 什么是mtl？
      *
      * Mtl是 Monad Transformer Library首字母缩略词。它的主要目的是使 monad 变换器嵌套更容易使用。它通过将大多数常见 monad 变换器的
      * 作用相互编码为类型类来实现这一点。要理解这意味着什么，我们首先要看一些常见的 monad 变换器。 接下来我将介绍一些鲜为人知的变换器
      * 'StateT'和'Kleisli'，如果您已经了解'StateT'和'Kleisli'，请随意跳过下一节。
      */


    /**
      * Kleisli
      *
      * 'Kleisli'允许我们从环境中读取并创建依赖于环境的新的值。这有时候特别有用,例如你需要从一些外部文件中读取配置。有些人喜欢将此描述
      * 为函数式编程的依赖注入。
      *
      * 举个例子，假设我们想要调用一个服务，但是为了进行调用，我们需要传递一些配置:
      * */

    object ReaderT_Example {
        /** 首先，定义一些引进和类型声明：*/
        import cats.data._
        import cats.effect._

        trait blueprint {
            type Config
            type Result

            /** 我们为要我们想要读取的配置和调用的服务定义两个函数。 */
            def getConfig: IO[Config]                   // getConfig: cats.effect.IO[Config]
            def serviceCall(c: Config): IO[Result]      // serviceCall: (c: Config)cats.effect.IO[Result]

            /**
              * 最简单的方法是从应用程序的最顶层开始传递配置。然而这可能是相当繁琐的，所以我们使用'Kleisli'。 'Kleisli'　为我们提供了
              * 'ask'功能，它允许我们访问类型为　'E'　的只读环境值：
              *
              * 　　def ask[F[_], A](implicit F: Applicative[F]): Kleisli[F, A, A] =　Kleisli(F.pure)
              *
              * 然后我们可以使用'flatMap'，'map'　或　for-comprehensions　来实际使用该值：
              **/
            def readerProgram: Kleisli[IO, Config, Result] = for {
                config <- Kleisli.ask[IO, Config]
                result <- Kleisli.liftF(serviceCall(config))
            } yield result // readerProgram: cats.data.ReaderT[cats.effect.IO,Config,Result]

            /**
              * 现在我们有一个 'Kleisli'，它返回我们要的结果，下一步是实际“注入”依赖。为此，'Kleisli[F，E，A]'为我们提供了一个'run'函数，
              * 它的意思是我们给它一个值'E'，然后返回一个'F[A]'，在我们的例子是一个'IO'：
              *
              * def run(e: E): F[A]
              *
              * 结合我们的'getConfig'目的，我们现在可以编写我们程序的入口点：
              * */
            def main: IO[Result] = getConfig.flatMap(readerProgram.run) // main: cats.effect.IO[Result]

            /**
              * 这就是我们如何在Scala中执行函数依赖注入。但是，我认为这种模式并不经常使用，因为它会强制您将所有步骤包装在“Kleisli”中。
              * 如果您继续阅读，我们将了解如何使用MTL缓解此问题。
              */
        }
    }

    /**
      * StateT
      *
      * 与'Kleisli'一样，'StateT'也允许我们从环境中读取值。但是与 Kleisli 不同，它允许我们写入环境并保持状态，因此而得名。通过'StateT'
      * 而不是'IO'，我们可以有意识地创建可以访问外部世界并且可以保持可变状态的程序。这是非常强大的，但是如果使用时不小心，它可以产生类似指令
      * 式编程类似的，在全局滥用可变状态和无限制的副作用的程序。小心使用“StateT”，它可以成为那些需要可变状态的应用程序的强大的工具。
      *
      * 一个常见的用例是向外部服务发送请求，并且在请求返回之后，使用结果值来修改用于创建下一个请求的环境。这个环境可以像缓存这样简单的东西，
      * 或者更复杂的东西，比如动态地改变每个请求的参数，取决于环境当前所处的状态。让我们看一个抽象的例子，展示这种能力。
      * */
    object StateT_Example {
        import cats.effect.IO
        import cats.data.StateT

        trait blueprint {
            type Env
            type Request
            type Response

            def initialEnv: Env

            /** 首先，我们将定义一个调用外部服务的函数，该函数将参考环境。 */
            def request(r: Request, env: Env): IO[Response]

            /** 接下来，我们还需要一个给出响应的函数，输入旧环境将返回一个新的环境。*/
            def updateEnv(r: Response, env: Env): Env

            /**
              * 现在我们可以开始使用'StateT'了。为此，我们将创建一个新的组合函数，该函数将使用当前环境发出请求，并在收到响应后更新它：
              *
              * StateT 的 get -> liftF -> modify 显式展示了对 StateT 的 A 和 S 的获取(S) -> 执行(A) -> 修改(S) 过程。
              * */
            def requestWithState(r: Request): StateT[IO, Env, Response] = for {
                // StateT.get 定义容器（如：IO，要和回容器类型齐返）并显式得到（由 run 传入的初始）环境状态 S
                env <- StateT.get[IO, Env]

                // liftF 在环境中执行参数 A（request(r, env)），得到返回值:IO[Response] (返回值的容器也要对齐)
                resp <- StateT.liftF(request(r, env))

                // modify 执行参数 updateEnv(resp, _) 来修改S，并将新的 S 保存进容器 IO (modify 不执行 A，因此没有返回值)
                _ <- StateT.modify[IO, Env](updateEnv(resp, _))
            } yield resp

            /**
              * 这个例子证明了'StateT'的力量。我们可以通过使用'StateT.get'（返回'StateT [IO，Env，Env]'类似于'ReaderT.ask'）
              * 获取当前状态，我们也可以使用'StateT.modify'修改它（需要函数'Env => Env' 并返回'StateT [IO，Env，Unit]'）。
              *
              * 现在，我们可以重复使用'requestWithState'函数N次：
              * */

            // 模拟些请求
            def req1: Request
            def req2: Request
            def req3: Request
            def req4: Request
            def stateProgram: StateT[IO, Env, Response] = for {
                resp1 <- requestWithState(req1)
                resp2 <- requestWithState(req2)
                resp3 <- requestWithState(req3)
                resp4 <- requestWithState(req4)
            } yield resp4

            /** 现在我们按照意愿完成了一个成熟的计划。但是，我们可以用'StateT'价值实际做些什么呢？要运行完整的程序，我们需要一个'IO'。
              * 当然，就像'ReaderT'一样，我们可以通过使用'run'方法将'StateT'转换为'IO'并为我们的环境提供初始值。我们试试吧！*/
            def main: IO[(Env, Response)] = stateProgram.run(initialEnv) // main: cats.effect.IO[(Env, Response)]

            /** 这为我们提供了有状态的应用程序。酷！接下来，我们将看看我们如何组合不同的变换器，以及 monad变换器是什么样的*/
        }
    }

    /**
      * 关于 Monad 变换器对作用进行编码的一些概念：
      *
      * 'EitherT'用于处理捕获异常的作用。'Kleisli'用于处理从环境中读取值的作用。 'StateT'用于处理本地可变状态的作用。
      *
      * 所有这些 monad 变换器都将它们的效果编码为数据结构，但还有另一种方法可以实现相同的结果：类型类！
      *
      * 例如，我们关注过的'Kleisli.ask'函数​​，如果我们在这里使用类型类，它会是什么样子？好吧，Cats-mtl 有一个实现，它被称为
      * 'ApplicativeAsk'。您可以将其视为编码为类型类的“Kleisli”：
      *
      *   trait ApplicativeAsk[F[_], E] {
      *     val applicative: Applicative[F]
      *     def ask: F[E]
      *   }
      *
      * 在它的内部'ApplicativeAsk'只是用于从环境中读取值实，就像'ReaderT'做的那样。同时也与“ReaderT”完全相同，它也包含一个表示
      * 该环境的类型参数“E”。
      *
      * 如果您想知道为什么'ApplicativeAsk'具有'Applicative'字段而不是仅仅从'Applicative'扩展，因为是为了避免因在范围内隐含地具有
      * 多个给定类型的子类（此处为'Applicative'）而产生的隐含歧义。所以在这种情况下，也因此我们喜欢组合而不是继承，否则，我们不能将
      * 'Monad'与'ApplicativeAsk'放在一起使用。您可以在Adelbert Chang的这篇优秀博客文章中阅读有关此问题的更多信息：
      * （https://typelevel.org/blog/2016/09/30/subtype-typeclasses.html）。
      * */

    /**
      * 类型类的“作用”
      *
      * 'ApplicativeAsk'是 Cats-mtl 的一个核心案例。 Cats-mtl 为大多数常见效果提供类型类，使您可以选择所需的效果，而无需实现特
      * 定的 monad 变换器栈。
      *
      * 理想情况下，您只使用具有不同类型类约束的抽象类型构造函数“F [_]”编写所有代码，然后在最后实现能够满足这些约束的特定数据类型的代
      * 码并运行之。
      *
      * 所以不用多说，让我们尝试将我们早期的'Reader'程序转换为mtl-style。首先，我将再次包括原始程序：
      * */
    object MTL_Example {
        import cats.Monad
        import cats.data._
        import cats.effect._
        import simulacrum.typeclass

        trait blueprint {
            type Config
            type Result

            def getConfig: IO[Config]                   // getConfig: cats.effect.IO[Config]
            def serviceCall(c: Config): IO[Result]      // serviceCall: (c: Config)cats.effect.IO[Result]

            /**
              * 现在我们应该用'F'替换'Kleisli'并添加'ApplicativeAsk[F，Config]'约束，对吧？但是我们有一个小问题，我们怎样才能将
              * 'serviceCall'这个'IO'值提升到我们抽象的'F'语境中？幸运的是，'cat-effect'已经定义了一个称为'LiftIO'的 trait 旨在
              * 帮助我们，它完全符合您的期望：
              *
              *   @typeclass trait LiftIO[F[_]] {
              *     def liftIO[A](io: IO[A]): F[A]
              *   }
              * */

            /**
              * 如果存在'LiftIO [F]'的实例，我们可以将任何'IO[A]'提升为'F[A]'。此外，'IO' 定义了一个方法'to'，它利用这个类型类来
              * 提供一些更友好的语法。
              *
              * 有了这些，我们现在可以使用 MTL 定义我们的'readerProgram'
              * */
            import cats.mtl.implicits._
            import cats.effect.IO
            import cats.effect.LiftIO
            import cats.mtl.ApplicativeAsk
            import cats.implicits._

            // F[_]: Monad: LiftIO == Monad[F[_]], LiftIO[F[_]]
            def readerProgram[F[_]: Monad: LiftIO](implicit A: ApplicativeAsk[F, Config]): F[Result] = for {
                config <- A.ask
                result <- serviceCall(config).to[F]  // LiftIO[F].liftIO[Result](serviceCall(config))
            } yield result

            /**
              * 我们将'Kleisli.ask'的调用替换为'ApplicativeAsk'提供的'ask'，而不是使用'Kleisli.liftF'去将'IO'提升到
              * 'Kleisli'中去．我们也可以简单地在'IO'上运行to，如果你想写的漂亮些。
              *
              * 现在运行它，我们需要做的就是指定要运行的目标'F'，在我们的例子中'Kleisli[IO，Config，Result]'完全适合：
              * */
            val materializedProgram = readerProgram[Kleisli[IO, Config, ?]]

            def main: IO[Result] = getConfig.flatMap(materializedProgram.run) // main: cats.effect.IO[Result]

            /**
              * 这种将一个具有抽象类型构造器（[_]: Monad: LiftIO）和类型类（A: ApplicativeAsk[F, Config]）的程序转换成
              * 一个具有实际数据类型的过程被称为"解释" 或 "物化"．
              * */
        }

        trait blueprint_v2 extends blueprint {
            import cats.effect.IO
            import cats.effect.LiftIO

             /**
              * 到目前为止虽然一切正常，但这似乎也并没有比以前更好很多。我一开始就开玩笑说，一旦你使用一个以上的 monad 变换器，就
              * 会感到 MTL 真的很棒。所以(为了证明这一点)，假设我们的程序现在需要能够处理错误（我认为这是一个非常合理的假设）。
              *
              * 要做到这一点，我们将使用'MonadError'，它在 cat-core 而不是 mtl 中，但从本质上讲，它编码了与 'EitherT' 共享
              * 的"短路"效果。
              *
              * 为了使事情变得简单，假设我们的配置在某种程度上是无效的，我们想要引发一个错误。为此，我们需要一个函数简单地验证返回
              * 'Config' 是有效或无效的：
              * */
            def validConfig(c: Config): Boolean

            /** 然后我们还要为我们的应用定义错误ADT：*/
            sealed trait AppError
            case object InvalidConfig extends AppError

            /** 现在我们可以从头开始扩展我们的程序。我们将添加一个'MonadError [F，AppError]' 类型别名'MonadAppError'，然后在我
              * 们的程序中为它添加一个约束。*/
            import cats.MonadError
            type MonadAppError[F[_]] = MonadError[F, AppError]

            /**
              * 我们可以做的另一件事是为'ApplicativeAsk [F，Config]'定义一个类型别名，这样我们就可以更容易地将它与上下文绑
              * 定语法一起使用：
              * */
            import cats.mtl.ApplicativeAsk
            type ApplicativeConfig[F[_]] = ApplicativeAsk[F, Config]

            /**
              * 现在我们想要以某种方式确保我们的配置有效，并且如果无效则引发'InvalidConfig'错误。为此，我们只需使用'MonadError'
              * 提供的'ensure'功能。它看起来像是这样的：
              *
              *   def ensure(error: => E)(predicate: A => Boolean): F[A]
              *
              * 它满足我们的需求：如果'predicate'函数返回'false'，它将引发传递的参数'error'。我们去尝试吧：
              * */
            import cats.implicits._
            def program[F[_]: MonadAppError: ApplicativeConfig: LiftIO]: F[Result] = for {
                config <- ApplicativeAsk[F, Config].ask
                        .ensure(InvalidConfig)(validConfig)    // MTL: 在 ApplicativeConfig 上堆叠 MonadAppError 类型类
                result <- serviceCall(config).to[F]
            } yield result

            /**
              * 很简单，现在让我们实现它吧！为此，我们将使用'Kleisli'，'EitherT' 和 'IO' 的 monad 堆栈。组合起来它看起来应该是:
              *
              * 　　IO[[AppError，Reader[Config，A]]]
              * */
            type EitherApp[A] = EitherT[IO, AppError, A]
            type Stack[A] = ReaderT[EitherApp, Config, A]
            //type Stack[A] = ReaderT[EitherT[IO, AppError, ?], Config, A]

            import cats.mtl.implicits._
            val materializedProgramStack: Stack[Result] = program[Stack]

            def main2: IO[Either[AppError, Result]] = EitherT.liftF(getConfig).flatMap(materializedProgramStack.run).value
            /**
              * 这就是 mtl 的神奇之处，它能够为堆栈中的每个 monad 变换器提供类型类实例。这意味着当我们堆叠'EitherT'，'ReaderT' 和
              * 'StateT' 时，你将能够获得 'MonadError'，'ApplicativeAsk' 和 'MonadState' 的实例，这非常有用！
              * */

            /*import cats._
            import cats.data._
            import cats.implicits._
            def monadErrorForReaderT[F[_], E, R](implicit F: MonadError[F, E]): MonadError[ReaderT[F, R, ?], E] =
                new MonadError[ReaderT[F, R, ?], E] {
                    def raiseError[A](e: E): ReaderT[F, R, A] =
                        Kleisli.liftF(F.raiseError(e))

                    def handleErrorWith[A](fa: ReaderT[F, R, A])(f: E => ReaderT[F, R, A]): ReaderT[F, R, A] =
                        Kleisli.ask[F, R].flatMap { r =>
                            Kleisli.liftF(fa.run(r).handleErrorWith(e => f(e).run(r)))
                        }
                }*/
        }
    }

    object Kleisli_liftT extends App{
        import cats.data.Kleisli
        import cats.effect.IO

        def LiftIO2Kleisli = {
            /** liftF(k) 只是 Kleisli(_ => k) 的语法糖 */
            val k:Kleisli[IO, Unit, String] = Kleisli.liftF(IO("Output from IO"))  // Kleisli(_ => IO("Output from IO"))
            println(k.run("Input").unsafeRunSync())
        }

        LiftIO2Kleisli
    }


    import com.sun.media.sound.InvalidDataException

    /**
      * Cats MTL(Monad Transformer Library) 是 Cats 的容器转换库
      * */
    object TestMTL extends App {
        import com.typesafe.config.ConfigFactory
        import com.typesafe.config.Config

        import cats.effect.IO
        import cats.Monad
        import cats.effect.LiftIO
        import cats.mtl.ApplicativeAsk
        import cats.implicits._
        import cats.data.Kleisli

        type ConfigItem = Int
        type Result = Boolean

        def getConfig(conf:Config): IO[ConfigItem] = IO{conf.getString("number.value").toInt}
        def compare(c:ConfigItem):Result = (110-100).abs > c


        /** Implement 1 */
        // F[_] == Kleisli[IO, Config, _]
        // F[_]: Monad: LiftIO == Monad[F[_]], LiftIO[F[_]]
        def readerProgram[F[_]: Monad: LiftIO](implicit A: ApplicativeAsk[F, ConfigItem]): F[Result] = for {
            config <- A.ask
            result <- IO.pure( compare(config) ).to[F]  // IO to Kleisli[IO, Config, ?]
        } yield result

        import cats.mtl.implicits._
        val meterializeProgram = readerProgram[Kleisli[IO, ConfigItem, ?]]  // type F == Kleisli[IO, Config, ?]

        println(getConfig(ConfigFactory.parseString(
            """
              | number.value : 100
            """.stripMargin)).flatMap(meterializeProgram.run).unsafeRunSync())   // False


        /** Implement 2: */
        import scala.util.{Try, Success, Failure}
        def compare(conf:Config):IO[Boolean] = IO( Try (
            Option(conf.getString("number.value")) match { case Some(v) => v.toInt }
        )).flatMap {
            case Success(c) => IO.pure((110-100).abs > c)
            case Failure(t) => IO raiseError t
        }

        println(compare(ConfigFactory.parseString("""
              | number.value : 100
            """.stripMargin)).unsafeRunSync)

        type L[A] = Either[Throwable, A]
        implicit def LiftIO2Either: LiftIO[L] = new LiftIO[L] {
            override def liftIO[A](ioa: IO[A]): L[A] = ioa.attempt.unsafeRunSync()
        }

        (implicitly[LiftIO[Either[Throwable, ?]]].liftIO(compare(ConfigFactory.parseString("""
              | number.value : 100
            """.stripMargin))) match {
            case Right(r) => IO(println(r))
            case Left(t) => IO(println(t.getMessage))
        }).unsafeRunSync()
    }


}
