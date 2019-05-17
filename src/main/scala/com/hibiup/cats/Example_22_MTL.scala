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

    package ReaderT_Example {
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
                // Kleisli.ask 从环境中取得(由 run 传入的)参数值, ask 的第一个参数是环境容器的类型,第二个是取出的入参类型.
                config <- Kleisli.ask[IO, Config]

                // Kleisli.liftF 读入一个(对齐的)容器, 提取出其中的值.
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
            def _main: IO[Result] = getConfig.flatMap(readerProgram.run) // main: cats.effect.IO[Result]

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
    package StateT_Example {
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
            def _main: IO[(Env, Response)] = stateProgram.run(initialEnv) // main: cats.effect.IO[(Env, Response)]

            /** 这为我们提供了有状态的应用程序。酷！接下来，我们将看看我们如何组合不同的变换器，以及 monad变换器是什么样的*/
        }
    }

    /**
      * 关于 Monad 变换器对作用进行编码的一些概念：
      *
      * 'EitherT' 用于处理捕获异常的作用。'Kleisli' 用于处理从环境中读取值的作用。 'StateT' 用于处理本地可变状态的作用。
      *
      * 所有这些 monad 变换器都将它们的效果编码为数据结构，但还有另一种方法可以实现相同的结果：类型类！
      *
      * 例如，我们使用过的 'Kleisli.ask' 函数​​，如果我们在这里使用类型类，它会是什么样子？好吧，Cats-mtl 有一个实现，它被称为
      * 'ApplicativeAsk'。您可以将其视为编码为类型类的 “Kleisli”：
      *
      *   trait ApplicativeAsk[F[_], E] {
      *     val applicative: Applicative[F]
      *     def ask: F[E]
      *   }
      *
      * 在它的内部 'ApplicativeAsk' 只是用于从环境中读取值实，就像 'Kleisli' 做的那样。同时也与 “Kleisli” 完全相同，它也包含
      * 一个表示该环境的类型参数“E”。
      *
      * 与 ApplicativeAsk.ask 直接返回 F[_] 不同的是 Kleisli.ask 返回的类型是 Kleisli[F, A, A]:
      *
      * 　　def ask[F[_], A](implicit F: Applicative[F]): Kleisli[F, A, A] =　Kleisli(F.pure)
      *
      * 如果您想知道为什么'ApplicativeAsk'具有'Applicative'字段而不是仅仅从'Applicative'扩展，因为是为了避免因在范围内隐含地
      * 具有多个给定类型的子类（此处为'Applicative'）而产生的隐含歧义。所以在这种情况下，也因此我们喜欢组合而不是继承，否则，我们不
      * 能将 'Monad' 与 'ApplicativeAsk' 放在一起使用。您可以在 Adelbert Chang 的这篇优秀博客文章中阅读有关此问题的更多信息：
      * （https://typelevel.org/blog/2016/09/30/subtype-typeclasses.html）。
      * */

    /**
      * 类型类的“作用”
      *
      * 'ApplicativeAsk'是 Cats-mtl 的一个核心案例。 Cats-mtl 为大多数常见效果提供类型类，使您可以选择所需的效果，而无需实现特
      * 定的 monad 变换器栈。
      *
      * 理想情况下，您只使用具有不同类型类约束的抽象类型构造函数“F[_]”编写所有代码，然后在最后实现能够满足这些约束的特定数据类型的代
      * 码并运行之。
      *
      * 所以不用多说，让我们尝试将我们早期的'Reader'程序转换为 mtl-style。首先，我将再次包括部分原始程序：
      * */
    package MTL_Reader_Example {
        import cats.Monad
        import cats.data._
        import cats.effect._

        trait blueprint {
            type Config
            type Result

            def getConfig: IO[Config]                   // getConfig: cats.effect.IO[Config]
            def serviceCall(c: Config): IO[Result]      // serviceCall: (c: Config)cats.effect.IO[Result]

            /**
              * 现在我们应该用 'F' 替换 'Kleisli' 并添加 'ApplicativeAsk[F，Config]' 约束，对吧？但是我们有一个小问题，我们怎
              * 样才能将 'serviceCall' 这个 'IO' 值提升到我们抽象的 'F' 语境中？幸运的是，'cat-effect' 已经定义了一个称为
              * 'LiftIO' 的 trait 旨在帮助我们，它完全符合您的期望：
              *
              *   @typeclass trait LiftIO[F[_]] {
              *     def liftIO[A](io: IO[A]): F[A]
              *   }
              * */

            /**
              * 如果存在 'LiftIO[F]' 的实例，我们可以将任何 'IO[A]' 提升为 'F[A]'。此外，'IO' 定义了一个方法 'to'，它利用这
              * 个类型类来提供一些更友好的语法。
              *
              * 有了这些，我们现在可以使用 MTL 定义我们的'readerProgram'
              * */
            import cats.mtl.implicits._
            import cats.effect.IO
            import cats.effect.LiftIO
            import cats.mtl.ApplicativeAsk
            import cats.implicits._

            // F[_]: Monad: LiftIO == Monad[F[_]], LiftIO[F[_]]
            // Monad[F[_]]  为 F 提供 FlatMap 隐式
            // LiftIO[F[_]] 为 F 提供 to[F] 或 liftIO 隐式
            def readerProgram[F[_]: Monad: LiftIO](implicit A: ApplicativeAsk[F, Config]): F[Result] = for {
                // ApplicativeAsk[F, Config].ask 等价于 Kleisli.ask[F, Config]，不同之处是 ApplicativeAsk 可以直接
                // 从环境中获得 F[_], 而不是 Kleisli[F, _, _]
                config <- /*Kleisli.ask[IO, Config]*/ A.ask

                // 将 IO[Result] 提升为 F[Result] 以对齐输出类型. 等价于：LiftIO[F].liftIO[Result](serviceCall(config))
                result <- serviceCall(config).to[F]
            } yield result

            /**
              * 我们将 'Kleisli.ask' 的调用替换为 'ApplicativeAsk' 提供的 'ask'，也不是使用 'LiftIO[F].liftIO' 去将 'IO'
              * 提升到 'Kleisli' 中去．我们可以简单地在 'IO' 上运行 to[F]，如果你想写的漂亮些。
              *
              * 现在运行它，我们需要做的就是指定要运行的目标 'F'，在我们的例子中 'Kleisli[IO，Config，Result]' 完全适合：
              * */
            // 给 for-comprehension 指定 Kleisli[IO, Config, ?] 类型参数, 它会将结果 IO[A] 装入 Kleisli[IO, Config, A]
            val materializedProgram = readerProgram[Kleisli[IO, Config, ?]]

            def _main: IO[Result] = getConfig.flatMap(materializedProgram.run) // main: cats.effect.IO[Result]

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
              * 要做到这一点，我们将使用 'MonadError'，它在 cat-core 而不是 mtl 中，但从本质上讲，它编码了与 'EitherT' 共享
              * 的"短路"效果。
              *
              * 为了使事情变得简单，假设我们的配置在某种程度上是无效的，我们想要引发一个错误。为此，我们需要一个函数简单地验证返回
              * 'Config' 是有效或无效的：
              * */
            def validConfig(c: Config): Boolean

            /** 然后我们还要为我们的应用定义错误ADT：*/
            sealed trait AppError
            case object InvalidConfig extends AppError

            /** 现在我们可以从头开始扩展我们的程序。我们将添加一个 'MonadError[E，AppError]' 类型别名 'MonadAppError'，然后在我
              * 们的程序中为它添加一个约束。*/
            import cats.MonadError
            type MonadAppError[Ehr[_]] = MonadError[Ehr, AppError]

            /**
              * 我们可以做的另一件事是为 'ApplicativeAsk [F，Config]' 定义一个类型别名，这样我们就可以更容易地将它与上下文绑
              * 定语法一起使用：
              * */
            import cats.mtl.ApplicativeAsk
            type ApplicativeConfig[F[_]] = ApplicativeAsk[F, Config]

            /**
              * 现在我们想要以某种方式确保我们的配置有效，并且如果无效则引发 'InvalidConfig' 错误。为此，我们只需使用 'MonadError'
              * 提供的 'ensure' 功能, 它为 F 提供了一个 ensure 函数, 看起来是这样的：
              *
              *   def ensure(error: => E)(predicate: A => Boolean): F[A]
              *
              * 如果 'predicate' 函数返回 'false'，它将引发传递的参数 'error'，否则返回 F 自己。我们去尝试吧：
              * */
            import cats.implicits._
            def program[F[_]: MonadAppError: ApplicativeConfig: LiftIO]: F[Result] = for {
                /**
                  * 由于在调用 program 的时候使用了 mtl 的隐式，使得 for-comprehension 在返回 F 也就是
                  * Kleisli[EitherT[IO, AppError, Result], Config, Result] 的时候具有了自动装箱能力。
                  * 因此只需要将注意力放在最终的返回类型 Result 上。
                  * */
                // 如果正确, 得到 ask 的值并装入: Kleisli[EitherT[IO, Nothing, ?], Config, A]
                // 否则抛出 AppError 最终得到 Kleisli[EitherT[IO, AppError, Nothing], Config, A]
                config <- ApplicativeAsk[F, Config].ask       // 输出类型 A == Config, 同时读出的值被装入 ? 处
                        .ensure(InvalidConfig)(validConfig)   // ensure 是 MonadError[U, AppError] 为 F 赋予的函数。
                                                              // 它具有为 U，也就是 EitherT 设置 AppError 的能力

                // 最终得到 Kleisli[EitherT[IO, Nothing, Result], Config, Result]
                result <- serviceCall(config).to[F]
            } yield result

            /**
              * 很简单，现在让我们实现它吧！为此，我们将使用 'Kleisli'，'EitherT' 和 'IO' 的 monad 堆栈。组合起来它看起来应该是:
              *
              * 　　IO[[AppError，Reader[Config，A]]]
              * */
            type EitherAppError[A] = EitherT[IO, AppError, A]
            type Stack[A] = Kleisli[EitherAppError, Config, A]   //type Stack[A] = Kleisli[EitherT[IO, AppError, ?], Config, A]

            /**
              * MTL: 让 for-comprehension 具有自动处理堆叠的数据类型类的能力. 如果堆栈（F）被注入了 MonadError 类型类，则堆栈中
              * 必须有 EitherT!
              * */
            import cats.mtl.implicits._
            val materializedProgramStack: Stack[Result] = program[Stack]    // F == Stack

            // EitherT.liftF 把一个带容器的值F[A]装入 Right．返回 Either[F, Nothing, A]
            def _main2: IO[Either[AppError, Result]] = EitherT.liftF(getConfig).flatMap(materializedProgramStack.run).value
            /**
              * 这就是 mtl 的神奇之处，它能够为堆栈中的每个 monad 变换器提供类型类实例。这意味着当我们堆叠 'EitherT'，'ReaderT' 和
              * 'StateT' 时，你将能够获得 'MonadError'，'ApplicativeAsk' 和 'MonadState' 的实例，这非常有用！
              * */


            /**********************************************************************
              *  如果你好奇它是如何具有这样的能力的，让我们来快速看一下一个具有 Kleisli 的 MonadError 实例：
              * */
            import cats.data._
            import cats.implicits._
            def monadErrorForReaderT[F[_], E, R](implicit me: MonadError[F, E]): MonadError[Kleisli[F, R, ?], E] =
                new MonadError[Kleisli[F, R, ?], E] {
                    def raiseError[A](e: E): Kleisli[F, R, A] = Kleisli.liftF(me.raiseError(e))
                    def handleErrorWith[A](fa: Kleisli[F, R, A])(f: E => Kleisli[F, R, A]): Kleisli[F, R, A] =
                        Kleisli.ask[F, R].flatMap { r =>
                            Kleisli.liftF(fa.run(r).handleErrorWith(e => f(e).run(r)))
                        }

                    override def flatMap[A, B](fa: Kleisli[F, R, A])(f: A => Kleisli[F, R, B]): Kleisli[F, R, B] = ???
                    override def tailRecM[A, B](a: A)(f: A => Kleisli[F, R, Either[A, B]]): Kleisli[F, R, B] = ???
                    override def pure[A](x: A): Kleisli[F, R, A] = ???
                }
            /**
              * 为了让 Kleisli[F, R, ?] 具备 MonadError 实例的能力，我们需要为 F 注入 MonadError. 这样我们就可以很容易为
              * 这个实例处理或抛出异常。注意，这意味着整个变换器栈必须具有抛出或捕获异常的能力，就像我们定义的这个一样。也就是说
              * 你只要在其中定义了 EitherT，你就具有了这样的提升能力。
              *
              * MTL 对　monad 堆栈中不同的类型类提升各自这些功能有不同的策略，但它们超出了本文的范围。
              *
              * 这对我们意味着，我们永远不必考虑如何在变换器堆栈中去提升某个单个 Monad。类型类机制使用的隐式搜索负责处理它。我觉得这很
              * 漂亮。现在让我们对比一下没有没有mtl编写的相同程序：
              * */
            def program2: Stack[Result] = for {
                config <- Kleisli.ask[EitherAppError, Config]
                _ <- if (validConfig(config)) ().pure[Stack]
                     else Kleisli.liftF[EitherAppError, Config, Unit](EitherT.leftT(InvalidConfig))
                result <- Kleisli.liftF(EitherT.liftF[IO, AppError, Result](serviceCall(config)))
            } yield result  // program2: Stack[Result]
            /**
              * 它是相同的程序，但现在我们必须添加类型注释，并且'liftF'　无处不在。如果您尝试删除其中一个类型注释，程序将无法编译，
              * 因此这是您需要的最小样板代码。
              ************************************************************************* */
        }

        object blueprint_v2_impl extends blueprint_v2 {
            override type Config = String
            override type Result = Int
            override def validConfig(c: Config): Boolean = ???
            override def getConfig: IO[Config] = ???
            override def serviceCall(c: Config): IO[Result] = ???
        }

        object app extends App {
            import blueprint_v2_impl._
            _main2
        }
    }

    package MTL_Option_1_Example {
        import cats.Monad
        import cats.data._
        import cats.effect._

        /**
          * MTL 支持对 Kleisli[IO, _, _] 的变换, 因此可以利用 MTL 实现自动变换.
          * */
        trait blueprint {
            type Config
            type Result

            def getConfig: IO[Config]               // getConfig: cats.effect.IO[Config]
            /** 1) 中间函数类型为 Option */
            def serviceCall(c: Config): /*IO*/Option[Result]      // serviceCall: (c: Config)cats.effect.IO[Result]

            import cats.effect.IO
            import cats.effect.LiftIO
            import cats.mtl.ApplicativeAsk
            import cats.implicits._

            def readerProgram[F[_]: Monad: LiftIO](implicit A: ApplicativeAsk[F, Config]): F[Result] = for {
                config <- A.ask
                /** 2) 利用缺省的 LiftIO 来实现中间过程的类型对齐，不需要额外实现一个。*/
                result <- IO[Result](serviceCall(config) match {
                    case Some(a:Result) => a
                }).to[F]
            } yield result

            /** 3) 使用 MTL 来实现 Kleisli[IO, _, _] 的自动变换 */
            import cats.mtl.implicits._
            val materializedProgram = readerProgram[Kleisli[IO, Config, ?]]

            def _main: IO[Result] = getConfig.flatMap(materializedProgram.run) // main: cats.effect.IO[Result]
        }
    }

    package MTL_Option_2_Example {
        import cats.Monad
        import cats.data._
        import cats.effect._

        /**
          * 如果将 Kleisli[IO, _, _] 替换成 Kleisli[Option, _, _], MTL 就无法支持.
          * */
        trait blueprint {
            type Config
            type Result

            /** 1) 输入类型为 Option */
            def getConfig: /*IO*/Option[Config]
            def serviceCall(c: Config): IO[Result]

            import cats.mtl.implicits._
            import cats.effect.IO
            import cats.effect.LiftIO
            import cats.mtl.ApplicativeAsk
            import cats.implicits._

            def readerProgram[F[_]: Monad: LiftIO](implicit A: ApplicativeAsk[F, Config]): F[Result] = for {
                config <- A.ask
                result <- serviceCall(config).to[F]
            } yield result

            /** 3)
              * 得不到 MTL 的支持,需要自己实现一个 LiftIO 将 IO 转换成 Kleisli[Option, _, _]
              * */
            implicit val LiftIO2Kleisli = new LiftIO[Kleisli[Option, Config, ?]] {
                override def liftIO[A](ioa: IO[A]): Kleisli[Option, Config, A] = ???
            }

            /** 2) MTL 不支持输出类型 Kleisli[Option, _, _], 需要提供隐式 LiftIO 来将中间过程的 IO 转成 Option */
            //import cats.mtl.implicits._
            val materializedProgram = readerProgram[Kleisli[/*IO*/Option, Config, ?]]


            def _main: Option[Result]/*: IO[Result]*/ = getConfig.flatMap(materializedProgram.run) // main: cats.effect.IO[Result]
        }
    }


    /**
      * 添加 State
      *
      * 接下来，让我们假设我们想要发送多个请求，并在下一个请求中使用前一个请求返回的信息，类似于我们之前在“StateT”示例中所做的操作。
      * 但是我们将使用 MonadState 替代 StateT:
      *
      *   trait MonadState[F[_], S] {
      *     val monad: Monad[F]
      *     def get: F[S]
      *     def set(s: S): F[Unit]
      *     def modify(f: S => S): F[Unit] = get.flatMap(s => set(f(s)))
      *   }
      * */
    package MTL_MonadState_Example {
        import cats.effect.IO
        import cats.effect.LiftIO
        import cats.Monad
        import cats.implicits._

        trait blueprint extends MTL_Reader_Example.blueprint_v2 {
            /**
              * 让我们假设我们有一个请求列表，我们希望在每个请求之后更新环境，并且我们还希望使用该环境来创建下一个请求。在最后，我们想要返回
              * 我们获得的所有响应的列表：
              */
            type Env
            type Request
            type Response

            type Result = List[Response]

            def updateEnv(r: Response, env: Env): Env
            def requests: List[Request]
            def newServiceCall(c: Config, req: Request, e: Env): IO[Response]

            /**
              * 到目前为止一切都好，接下来我们将使用'MonadState'来创建一个新函数，它将使用'updateEnv'修改环境以供'newServiceCall'使用。
              * 为此，我们将为'MonadState[F，Env]'创建一个新类型别名：
              * */
            import cats.mtl.MonadState
            type MonadStateEnv[F[_]] = MonadState[F, Env]

            def requestWithState[F[_]: Monad: MonadStateEnv: LiftIO](c: Config, req: Request): F[Response] = for {
                /**
                  * 在这里，我们使用'get'来检索环境的当前状态，然后我们使用'newServiceCall'并将其提升为'F'并用返回内容来修改环境
                  * (通过 'updateEnv')。
                  * */
                env <- MonadState[F, Env].get
                response <- newServiceCall(c, req, env).to[F]
                _ <- MonadState[F, Env].modify(updateEnv(response, _))
            } yield response

            /**
              * 现在，我们可以在请求列表中使用'requestWithState'，并将这个新部件嵌入到我们的程序中。最好的方法当然是'遍历'，因为我们
              * 可以利用 'Request => F[Response]' 实现从'List[Request]' 到 'F[List[Response]]' 的转变。所以不用多说，这
              * 是我们的最终程序，它使用了我们在本文中学到的所有三种不同的mtl类型类：
              * */
            import cats.mtl.ApplicativeAsk
            def program[F[_]: MonadAppError: MonadStateEnv: ApplicativeConfig: LiftIO]: F[Result] = for {
                config <- ApplicativeAsk[F, Config].ask
                        .ensure(InvalidConfig)(validConfig)
                responses <- requests.traverse(req => requestWithState[F](config, req))
            } yield responses

            /**
              * 这就是全部！当然，我们仍然必须运行它，所以让我们将'F'实现为适当的数据类型。我们将使用一堆'EitherT'，'StateT'和'
              * ReaderT'，'IO'作为我们的基础来满足'LiftIO'：
              * */
            import cats.data._
            import cats.mtl.implicits._
            def materializedProgram3 = program[StateT[EitherT[Kleisli[IO, Config, ?], AppError, ?], Env, ?]]

            /** 现在我们有一个完全适用的变换器堆栈。唯一剩下的就是通过运行各个层将该堆栈重新转换为“IO”。*/
            def initialEnv: Env
            def _main3: IO[Either[AppError, (Env, Result)]] =  getConfig.flatMap(conf =>
                    materializedProgram3.run(initialEnv) //Run the StateT layer
                            .value //Run the EitherT layer
                            .run(conf) //Run the ReaderT layer
                )

            /*********************
              *  如果调整容器的顺序 */
            //def materializedProgram4 = program[StateT[Kleisli[EitherT[IO, AppError, ?], Config, ?], Env, ?]]

            /** 执行顺序也需要调整 */
            //def _main4: IO[Either[AppError, (Env, Result)]] =  getConfig.flatMap(conf =>
            //    materializedProgram4.run(initialEnv)
            //            .run(conf) //Run the ReaderT layer
            //            .value //Run the EitherT layer
            //)

            /**
              * 如果我们只使用变换器而不是 mtl 来获得相同的值，那么样板代码的数量将是极其难以忍受的。对于每个 monad 变换器和几十种类型
              * 的注释，我们需要多个'liftF'，将实际代码隐藏在样板代码的下面。
              *
              * 使用 Cats-mtl，处理不同的效果很简单，没有样板。我们可以将我们的应用程序描述为处理抽象上下文'F[_]'的函数，它必须能够提
              * 供对某些效果的约束。这些约束由 Cats-mtl 中的不同 MTL 类型类提供，并且它们的实例可以通过 Cats-mtl 的底层机制提升到最
              * 高层。
              *
              * 总之，Cats-mtl 提供了两个东西：表示效果的 MTL 类型类和通过变换器堆栈提升这些类的实例的方法。如果您想了解更多有关
              * Cats-mtl 的信息，请查看Cats-MTL 官方网站: https://typelevel.org/cats-mtl/
              * */
        }

        /**
          * 其他 mtl 类实例
          *
          * 我之前说 'ApplicativeAsk' 是 'Kleisli' 的类型类，但它绝不是唯一的实例的。已知 Monad 变换器堆栈非常不可靠，特别是在JVM
          * 上，因此有一些替代解决方案。例如 "Arrows" (https://github.com/traneio/arrows)，除了输出类型'Arrow[A，B]'之外，它还
          * 提供具有输入类型的效果类型。如果你稍微眯一下，它实际上相当于一个函数'A => IO[B]' 或 'ReaderT[IO，A，B]'。然而，与此同时
          * 它的效率更高。
          *
          * 其他示例包括使用类似 'cat-effect' 的 'Ref' 来代替 'MonadState'（可在此处找到例子:https://github.com/oleg-py/meow-mtl），
          * 或使用包含额外的错误类型参数的 bifunctor 'IO'，即 'BIO[E，A]' 来代替 'EitherT [IO，E，A]'（可以在这里找到例子:
          * https://github.com/LukaJCB/cats-bio）。
          *
          * 通常，我们可以通过使用更特殊的数据结构来为我们的效果类型类实例设计更有效的解决方案。 Monad 变换器非常通用，这使它们非常灵活，
          * 但这种灵活性可能需要付出代价。关于'mtl'的一个好处是我们不必费时去事先选择，而只是在我们的程序运行时才选择。例如，我们可能会在
          * 开发应用程序时选择某个 monad 变换器。然后，当我们想要扩展时，我们可以简单地通过对的实现程序更改几行代码就转换到新的数据结构。
          *
          * 从长远来看，我想提供一个 'cats-mtl' 子模块，它为每种效果类型组合提供非常专业和高效的数据类型。为此，我不久前创建了
          * cat-mtl-special 库，但它仍然是一项正在进行中的工作。同时也向 Jamie Pullar 致敬，他一直在生产中广泛使用 cat-mtl，
          * 并且还建立了一些更高性能的实例以及一些标准，你可以在这里找到他说过的话
          * （https://www.slideshare.net/RyanAdams12/jamie-pullar-cats-mtl-in-action/39）。
          * */
    }


    object Kleisli_liftT extends App{
        import cats.data.Kleisli
        import cats.effect.IO

        def LiftIO2Kleisli = {
            def i = Kleisli.ask[IO, String]
            println(i.run("Input").unsafeRunSync)

            /** liftF(k) 只是 Kleisli(_ => k) 的语法糖 */
            def k:Kleisli[IO, Unit, String] = Kleisli.liftF(IO("Output from IO"))  // Kleisli(_ => IO("Output from IO"))
            println(k.run().unsafeRunSync())
        }

        LiftIO2Kleisli
    }


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
