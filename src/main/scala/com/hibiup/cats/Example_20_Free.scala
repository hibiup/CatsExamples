package com.hibiup.cats


/**
  * 翻译自：https://typelevel.org/cats/datatypes/freemonad.html
  *
  * 从数学的角度来说，一个 free monad（至少在编程语言环境中）是一个左伴随的定义域是　Monads，共域是自函子（Endofunctors）的
  * 结构（Monads -> Endofunctors）所投射出的“健忘”（Forgetful）的函子（Endofunctors）.
  *
  * 具体地说：Free 是一个足够聪明的结构，它允许我们从任何仿函数(functor)构建出一个简单的 Monad。
  *
  * 这个所谓“健忘”的函子（Forgetful functor）它接受一个 Monad 输入：
  *
  *   1）忽略它的 monadic (flatMap) 部分
  *   2）忽略它的 pointed (pure) 部分
  *   3）只保留了 functor (map) 部分
  *
  * 也就是说，如果我们反转箭头（Endofunctors -> Monads）倒推它的左伴随（left adjoint）, 我们就可以得到 Free monad：
  *
  *   1）输入一个 functor
  *   2）添加 pointed 部分
  *   3）添加 monadic 行为
  *
  * 因此从实现的角度来看，从一个 functor 构建出一个 monad 可以通过以下步骤：
  *
  *   sealed abstract class Free[F[_], A]
  *   final case class Pure[F[_], A](a: A) extends Free[F, A]
  *   final case class Suspend[F[_], A](a: F[Free[F, A]]) extends Free[F, A]
  *
  * （这概括了定点仿函数的概念）
  *
  * 以上代码表达了:
  *
  *   1) Pure 通过 A 值构造出 Free 的实例（它是 pure 函数的具体化）
  *   2) Suspend 通过向 F 传递一个已存在的 Free 构造出新的 Free（它是 flatMap 函数的具体化）
  *
  * 所以一个典型的 Free 结构看起来如下：
  *
  *   Suspend(F(Suspend(F(Suspend(F(....(Pure(a))))))))
  *
  * 可以看出 Free 是一个递归结构，它使用 F[A] 中的 A 作为具有终端元素 Pure 的递归“载体”
  *
  * 从计算的角度来看，Free 的递归结构可以看作是一系列操作：
  *
  *   1）Pure 返回 A 值并结束整个计算。
  *   2）Suspend 是持续（递归）的；它用一个Suspension functor（https://topospaces.subwiki.org/wiki/Suspension_functor）F
  *   （例如可以是一个命令）暂停当前计算并将控制权交给呼叫者。A 表示与此计算绑定的值。
  *
  * 请注意，这种自由（Free）构造具有有趣的品质，它可以在堆上递归编码，而不像经典函数调用那样在堆上进行编码。这提供了我们之前听说过的
  * 堆栈安全性，允许安全运行非常大的自由结构体。
  *
  * 对于具有非常好奇心的人来说：
  *
  * 如果你观察 Cats 的实现，又将会看到另一个 ADT 成员 FreeT：
  *
  *   case class FlatMapped[S[_], B, C](c: Free[S, C], f: C => Free[S, B]) extends Free[S, B]
  *
  * FlatMapped 表示调用子例程 c，当 c 完成后，它将 c 的结果传递给函数 f 继续计算。
  *
  * 它实际上是 Free 结构的优化，允许在非常深的递归 Free 计算中解决隐含的二元复杂性问题。
  *
  * 这与重复附加到 List[_] 完全相同，随着操作序列变得越长，flatMap完成整个结构的“穿越”也就越慢。而使用 FlatMapped，Free 成为一个右
  * 相关的结构，不再受二元复杂性的影响。
  *
  */

/**
  * Free 实例:
  *
  * Free 程序由三部分组成：描述（description），实现（implementation） 和关注分离（separation of concern）：
  *
  * 1）描述（description）：用 ADT（algebraic data type）来抽象行为。也就是定义 A.
  * 2）关注分离（separation of concern）：实现 suspend，即用 DSL 来实现业务流程。
  * 3）实现（implementation）：实现 Pure, 得到左伴随(Monad). 即根据 ADT 得到 Monad。
  *
  * 使用 Cats Free 需要引入 cats-free 模块
  *
  *  libraryDependencies ++= Seq(
  *    "org.typelevel" %% "cats-free" % catsVersion
  *    ...
  *  )
  *
  * */
object Example_20_Free {
    def free_monad_example(): Unit = {
        /** 这个例子演示通过 Free 操作一个键值对 Storage 对象*/

        /** 1-1) 功能描述：也就是定义 ADT（ADT：Algebraic Data Type），也就是用代数类型来描述运算 */
        sealed trait KVStoreA[A]
        final case class Put[T](key: String, value: T) extends KVStoreA[Unit]
        final case class Get[T](key: String) extends KVStoreA[Option[T]]
        final case class Delete(key: String) extends KVStoreA[Unit]

        /** 1-2) 定义 KVStoreA 的 Free 类型投影 */
        import cats.free.Free
        type KVStore[A] = Free[KVStoreA, A]

        /** 1-3）DSL，通过 liftF 将 KVStoreA 的 ADT 提升到其 Free 类型的投影. */
        import cats.free.Free.liftF
        def put[T](key: String, value: T): KVStore[Unit] = liftF[KVStoreA, Unit](Put[T](key, value))
        def get[T](key: String): KVStore[Option[T]] = liftF[KVStoreA, Option[T]](Get[T](key))
        def delete[T](key: String): KVStore[Unit] = liftF[KVStoreA, Unit](Delete(key))
        // Update composes get and set, and returns nothing.
        def update[T](key: String, f: T => T): KVStore[Unit] =
            for {
                vMaybe <- get[T](key)
                _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
            } yield ()


        /**
          * 接下来我们需要通过以下几个步骤来 "freeing" 这些 ADT:
          *
          * 2) 关注分离：用 DSL 来描述业务运算
          * */
        def program: KVStore[Option[Int]] =
            for {
                _ <- put("wild-cats", 10)
                _ <- update[Int]("wild-cats", (_ + 12))
                _ <- put("pet-cats", 2)
                n <- get[Int]("wild-cats")  // Option(22)
                _ <- delete("pet-cats")
            } yield n  // Free{"wild-cats", 10 ... } == KVStoreA{"wild-cats", 10 ... }

        /**
          * program 看上去貌似一个 monadic flow，但是事实上这里只是构建了一个递归运算的结构（分离了关注），也就是说 Free 只是
          * 被用于这些嵌入式 DSL 来构建执行流程的，这个流程并不能被直接执行。如果我们试图运行 program，实际上只会得到一个
          * Free[_] 结构，得不到期待的结果：
          * */
        println(program)  // Free[...]


        /** 3) 功能实现(Pure)：
          *
          * 因此接下来我们需要一个能够执行这个流程的“编译器”，也就是实现左伴随 F[_] -> G[_] 过程，得到真正的 Monad。
          * Cats 为此提供了一个 FunctionK[F, G] 函数(syntax 语法是 ~> ) 来封装这个过程.
          * */
        // import cats.arrow.FunctionK
        import cats.{Id, ~>}
        import scala.collection.mutable
        def impureCompiler: KVStoreA ~> Id /*FunctionK[KVStoreA,Id]*/ =
            new (KVStoreA ~> Id) {
                // a very simple (and imprecise) key-value store
                val kvs = mutable.Map.empty[String, Any]
                def apply[A](fa: KVStoreA[A]): A =
                    fa match {
                        case Put(key, value) =>
                            println(s"put($key, $value)")
                            kvs(key) = value
                            ()     // 不必担心这里的误报
                        case Get(key) =>
                            println(s"get($key)")
                            kvs.get(key)  // .map(_.asInstanceOf[A])
                        case Delete(key) =>
                            println(s"delete($key)")
                            kvs.remove(key)
                            ()
                    }
            }

        /** 4）执行 */
        val result: Option[Int] = program.foldMap(impureCompiler)
        println(result)  // Option(22)


        /** 3-1）也可以用 State monad 来实现这个编译器　*/
        import cats.data.State
        type KVStoreState[A] = State[Map[String, Any], A]    // 因为可能得到 A, 也可能得到 Option[A]，所以只能用 Any
        val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {
            def apply[A](fa: KVStoreA[A]): KVStoreState[A] = fa match {
                    case Put(key, value) => State.modify(_.updated(key, value))
                    case Get(key) => State.inspect(_.get(key).map(_.asInstanceOf[A]))
                    case Delete(key) => State.modify(_ - key)
                }
        }

        /** 4-1）执行 */
        val result2: (Map[String, Any], Option[Int]) = program.foldMap(pureCompiler).run(Map.empty).value
        assert(result2._2 == result)  // result2: (Map[String,Any], Option[Int]) = (Map(wild-cats -> 22),Some(22))
    }


    def free_interact(): Unit = {
        /** 1）功能描述：定义 ADT 来描述功能 */
        sealed trait Interact[A]
        case class Ask(prompt: String) extends Interact[String]
        case class Tell(msg: String) extends Interact[Unit]

        sealed trait DataOp[A]
        case class AddCat(a: String) extends DataOp[Unit]
        case class GetAllCats() extends DataOp[List[String]]

        /** lift 到 DSL */
        import cats.free.Free
        import cats.InjectK
        class Interacts[F[_]](implicit I: InjectK[Interact, F]) {
            def tell(msg: String): Free[F, Unit] = Free.inject[Interact, F](Tell(msg))
            def ask(prompt: String): Free[F, String] = Free.inject[Interact, F](Ask(prompt))
        }
        object Interacts {
            implicit def interacts[F[_]](implicit I: InjectK[Interact, F]): Interacts[F] = new Interacts[F]
        }

        class DataSource[F[_]](implicit I: InjectK[DataOp, F]) {
            def addCat(a: String): Free[F, Unit] = Free.inject[DataOp, F](AddCat(a))
            def getAllCats: Free[F, List[String]] = Free.inject[DataOp, F](GetAllCats())
        }
        object DataSource {
            implicit def dataSource[F[_]](implicit I: InjectK[DataOp, F]): DataSource[F] = new DataSource[F]
        }

        /** 2）分离关注点 */
        import cats.data.EitherK
        type CatsApp[A] = EitherK[DataOp, Interact, A]
        def program(implicit I : Interacts[CatsApp], D : DataSource[CatsApp]): Free[CatsApp, Unit] = {
            import I._, D._
            for {
                cat <- ask("What's the kitty's name?")
                _ <- addCat(cat)
                cats <- getAllCats
                _ <- tell(cats.toString)
            } yield ()
        }

        /** 3) 功能实现 */
        import scala.collection.mutable.ListBuffer

        import cats.{~>, Id}
        object ConsoleCatsInterpreter extends (Interact ~> Id) {
            def apply[A](i: Interact[A]) = i match {
                case Ask(prompt) =>  println(prompt); readLine()
                case Tell(msg) => println(msg)
            }
        }

        object InMemoryDatasourceInterpreter extends (DataOp ~> Id) {
            private[this] val memDataSet = new ListBuffer[String]
            def apply[A](fa: DataOp[A]) = fa match {
                case AddCat(a) => memDataSet.append(a); ()
                case GetAllCats() => memDataSet.toList
            }
        }

        val interpreter: CatsApp ~> Id = InMemoryDatasourceInterpreter or ConsoleCatsInterpreter

        /** 4) 执行 */
        val evaled: Unit = program.foldMap(interpreter)
    }

    /**
      * FreeT
      *
      * 通常我们想要在构建 Free monad 时交织语法树，而某些效果未被声明为 ADT。 FreeT 通过允许我们将 AST 的构建步骤与
      * 其他基本 monad 中的调用操作混合来解决这个问题。
      *
      *　在以下示例中，显示了一个基本控制台应用程序。当用户输入一些文本时，我们使用单独的State monad来跟踪用户键入的内容
      *
      * 正如我们在这种情况下可以观察到的那样，FreeT　为我们提供了一种替代方案，可以将 state monad 委托给更强的等价机制，
      * 而不是在我们自己的 ADT 中模拟 State 运算
      * */

    def freeT_example(): Unit = {
        import cats.free._
        import cats._
        import cats.data._

        /** 1) 定义一个没有 state 语义的 ADT 基类 */
        sealed abstract class Teletype[A] extends Product with Serializable
        /** 1-1) 继承这个基类 */
        final case class WriteLine(line : String) extends Teletype[Unit]
        final case class ReadLine(prompt : String) extends Teletype[String]

        /** 1-2) 使用 FreeT transformer */
        type TeletypeT[M[_], A] = FreeT[Teletype, M, A]
        type Log = List[String]
        type TeletypeState[A] = State[List[String], A]

        /** 1-3) 用 FreeT.liftF 来提升 transformer, 得到 Teletype 的 Free 结构. */
        object TeletypeOps {
            def writeLine(line : String) : TeletypeT[TeletypeState, Unit] =
                FreeT.liftF[Teletype, TeletypeState, Unit](WriteLine(line))
            def readLine(prompt : String) : TeletypeT[TeletypeState, String] =
                FreeT.liftF[Teletype, TeletypeState, String](ReadLine(prompt))
            def log(s : String) : TeletypeT[TeletypeState, Unit] =
                FreeT.liftT[Teletype, TeletypeState, Unit](State.modify(s :: _))
        }

        /** 2) 实现关注分离 */
        def program : TeletypeT[TeletypeState, Unit] = {
            for {
                userSaid <- TeletypeOps.readLine("what's up?!")
                _ <- TeletypeOps.log(s"user said : $userSaid")
                _ <- TeletypeOps.writeLine("thanks, see you soon!")
            } yield ()
        }

        /** 3) Pure: 直接用 StateT 实现运算实体. */
        def interpreter = new (Teletype ~> TeletypeState) {
            def apply[A](fa : Teletype[A]) : TeletypeState[A] = {
                fa match {
                    case ReadLine(prompt) =>
                        println(prompt)
                        val userInput = "hanging in here" //scala.io.StdIn.readLine()
                        StateT.pure[Eval, List[String], A](userInput)
                    case WriteLine(line) =>
                        StateT.pure[Eval, List[String], A](println(line))
                }
            }
        }

        import TeletypeOps._

        /** 4) 执行 */
        val state = program.foldMap(interpreter)
        // state: TeletypeState[Unit] = cats.data.IndexedStateT@69c0c213

        val initialState = Nil
        // initialState: scala.collection.immutable.Nil.type = List()

        val (stored, _) = state.run(initialState).value
        // what's up?!
        // thanks, see you soon!
        // stored: List[String] = List(user said : hanging in here)
    }
}
