package com.hibiup.cats

package Example_23_MonadError {
    import cats._
    import cats.data._
    import cats.implicits._

    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global

    /**
      * 翻译：https://www.codacy.com/blog/monad-error-for-the-rest-of-us/
      *
      * Scala 的 Either 类型允许我们的程序存在两个分支：左分支或右分支。 Futures 也提供了同样的能力 (Success or Failure). 综合考虑
      * 两者，Future 和 Either 公共提供了三个分支：(Success-Left, Success-Right, Failure) 但是我们很容易遗漏第三个分支（Failure，
      * Scala 不建议 Future 返回 Failure）Cats' MonadError 允许我们通过将 Failure 转换成 Success-Left 从而将三个分支
      * (Success-Left, Success-Right, Failure) 缩减为两 (Left or Right)
      * */
    trait MonadErrorBluePrint {
        type User
        type Order

        sealed trait ServiceError{
            def msg:String
        }
        case class UserNotFound(msg:String) extends ServiceError
        case class OrderNotFound(msg:String) extends ServiceError

        def getUserId: Either[UserNotFound, Int]
        def fetchUser(id: Int): Future[Either[UserNotFound, User]]
        def fetchOrder(user: User): Future[Either[OrderNotFound, Order]]

        def getOrderForUser:EitherT[Future, ServiceError, Order] = for {
            /**
              * 接下来的代码我们期待返回 Either[ServiceError, _]，但是实际上却遗漏了 Failure 的情况. 除非我们在每一个
              * Future后面都加上 .recover 方法.比如:
              *
              *   EitherT(fetchUser(userId).recover{ case t => Left(UserNotFound(t.getMessage) })
              *   EitherT(fetchOrder(user).recover{case t => Left(OrderNotFound(t.getMessage)) }
              * */
            userId <- EitherT.fromEither[Future](getUserId: Either[ServiceError, Int])
            user <- EitherT(fetchUser(userId): Future[Either[ServiceError, User]])
            order <- EitherT(fetchOrder(user): Future[Either[ServiceError, Order]])
        } yield order


        /**
          * 为每一个函数都加上 recover 非常麻烦, 因此，我们需要 Monad Transformer 处理这种边缘情况。MonadError 可以通过 recoverWith
          * 为整个加上 Failure 处理, 将结果转成 EitherT
          * */
        val recoveredEitherT = MonadError[EitherT[Future, ServiceError, ?], Throwable].recoverWith(getOrderForUser) {
            case t => EitherT.leftT[Future, Order](new ServiceError {
                override def msg: String = t.getMessage
            })
        }

        /** MonadError 的 apply 签名如下:
          *
          *   def apply[F[_], E](implicit F: MonadError[F, E]): MonadError[F, E] = F
          *
          * 它期待一个 F[_] 参数作为目标容器（通常是 EitherT），和一个 E 类型的错误(通常是 Throwable)。但是因为 EitherT 的参数签名
          * 是：EitherT[G[_], B, A],而我们只关心正常，也就是第三个参数（A），所以我们可以缩减它，这样 recoveredEitherT 就可以简写为：
          * */
        type MyEitherT[A] = EitherT[Future, ServiceError, A]
        val recoveredEitherT2 = MonadError[MyEitherT, Throwable].recoverWith(getOrderForUser) {
            case t => EitherT.leftT[Future, Order](new ServiceError {
                override def msg: String = t.getMessage
            })
        }

        /**
          * 利用 context bound，可以写的更通用一些。
          *
          * cats.implicits._ 提供了对 MonadError 的隐式注入
          *
          *   val me = implicitly[MonadError[Future, Throwable]]
          *
          * */
        implicit class RecoveringEitherT[F[_], A, B](underlying: EitherT[F, A, B])(implicit me: MonadError[F, Throwable]) {
            /** 为 EitherT 添加 recoverF 方法, 从一个 Throwable 中返回 Left (通过 MonadError 的 recoverWith 方法)*/
            def recoverF(op: Throwable => A) = MonadError[EitherT[F, A, ?], Throwable].recoverWith(underlying) {
                case t => EitherT.fromEither[F](op(t).asLeft[B])
            }
        }

        val recoveredEitherT3 = getOrderForUser.recoverF(t => new ServiceError {
            override def msg: String = t.getMessage
        })
    }

    object MonadErrorExample extends MonadErrorBluePrint with App{
        override type User = String
        override type Order = String

        override def getUserId: Either[UserNotFound, Int] = 1.asRight
        override def fetchUser(id: Int): Future[Either[UserNotFound, String]] = ???
        override def fetchOrder(user: String): Future[Either[OrderNotFound, String]] = ???

        println(recoveredEitherT3)
    }
}
