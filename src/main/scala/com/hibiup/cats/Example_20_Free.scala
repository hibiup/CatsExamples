package com.hibiup.cats


/**
  * 参考：https://typelevel.org/cats/datatypes/freemonad.html
  *
  * 使用 Cats Free 需要引入 cats-free 模块
  *
  * libraryDependencies ++= Seq(
  * "org.typelevel" %% "cats-free" % catsVersion
  * ...
  * )
  *
  * Free 程序的特点是描述（description），实现（implementation）和关注分离（separation of concern），因此在实现过程中有以下定式：
  *
  * 1）用 ADT（algebraic data type）来作为描述（description）部分，既然是描述，我们很自然会联想到 DSL
  * 2）算法即用 DSL 描述的功能的具体实现部分
  * 3）关注点分离，指的是用 DSL 来描述业务流程
  *
  * */
object Example_20_Free {
    def free_monad_example(): Unit = {
        /** 这个例子演示通过 Free 存储一个键值对 Storage */

        /** 1) 功能描述：也就是定义 ADT（ADT：Algebraic Data Type），也就是用代数类型来描述这个存储的运算 */
        sealed trait KVStoreA[A]
        final case class Put[T](key: String, value: T) extends KVStoreA[Unit]
        final case class Get[T](key: String) extends KVStoreA[Option[T]]
        final case class Delete(key: String) extends KVStoreA[Unit]

        /** 接下来我们需要通过以下几个步骤来 "freeing" 这些 ADT:
          *
          * 2-1) 得到 KVStoreA 的 Free 类型投影
          * */
        import cats.free.Free
        type KVStore[A] = Free[KVStoreA, A]

        /** 2-2) 定义 DSL, 依然是功能描述. 作用是通过 liftF 将对 KVStoreA 的 ADT 提升到对其 Free 类型的投影. */
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

        /** 2-3) 关注分离：用 Free 类行投影出的 DSL 函数来描述业务运算 */
        def program(): KVStore[Option[Int]] =
            for {
                _ <- put("wild-cats", 10)
                _ <- update[Int]("wild-cats", (_ + 12))
                _ <- put("pet-cats", 2)
                n <- get[Int]("wild-cats")  // Option(22)
                _ <- delete("pet-cats")
            } yield n  // Free{"wild-cats", 10 ... } == KVStoreA{"wild-cats", 10 ... }

        /** 3) 功能实现：
          *
          * program 看上去貌似一个 monadic flow，但是事实上这里只是构建了一个递归运算的结构（分离了关注），也就是说 Free 只是
          * 被用于这些嵌入式 DSL 来构建执行流程的，这个流程并不能被直接执行。如果我们试图运行 program，实际上只会得到一个
          * Free[_] 结构，得不到期待的结果：
          * */
        println(program())  // Free[...]

        /** 因此接下来我们需要一个能够执行这个流程的“编译器”，也就是实现 F[_] -> G[_] 过程。Cats 为此提供了一个 FunctionK[F, G]
          * 函数(syntax 语法是 ~> ) 来封装这个过程. */
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
                            kvs.get(key).map(_.asInstanceOf[A])
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
}
