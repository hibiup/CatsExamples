package com.hibiup.cats


/**
  * 如果同时存在两个 DSL 系统，例如：UserRepository　和 EmailService, 比较 Free 和 Tagless 的不同:
  *
  * https://softwaremill.com/free-tagless-compared-how-not-to-commit-to-monad-too-early/
  */

package Example_22_Tagless {

    import java.util.UUID

    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration._

    /**
      * Free version
      *
      * Free 是基于 ADT 工作的，因此要在设计时定义出 Coproduct 和 Product.
      */
    package FreeVersion {

        trait design {
            import cats.free.Free

            implicit val ec = scala.concurrent.ExecutionContext.global

            // Types
            type Point
            type ID
            type User

            /** 需要预先设计 Coproduct */
            // ADT for UserRepository
            sealed trait UserRepository[+T]
            /** 以下 SendEmail 要和 FindUser 等工作在一起，也必须继承自 UserRepository */
            // ADT for EmailService
            // sealed trait EmailService[+T]   /** 无法使用 */
            /** 除非使用 Coproduct 结构作为两者的超集, 并将之作为共同的容器.(Coproduct 是 Cats 的 Either) */
            // type UserAndEmailAlg[T] = Coproduct[UserRepository[T], EmailService[T], T]

            // Free for UserRepository
            type UserRepositoryFree[T] = Free[UserRepository, T]
            // Free for EmailService  /** 无法使用 */
            // type EmailServiceFree[T] = Free[EmailService, T]

            // Case class for User
            final case class FindUser(id: ID) extends UserRepository[Option[User]]
            final case class UpdateUser(u: User) extends UserRepository[Unit]
            // Case class for Email
            final case class SendEmail(email: String, subject: String, body: String) extends UserRepository[Unit]

            // Free for User
            def findUser(id: ID): UserRepositoryFree[Option[User]] = Free.liftF(FindUser(id))
            def updateUser(u: User): UserRepositoryFree[Unit] = Free.liftF(UpdateUser(u))
            // Free for Email
            def sendEmail(email: String, subject: String, body: String): UserRepositoryFree[Unit] =Free.liftF(SendEmail(email, subject, body))

            /*
             * Free Composite (DSL)
             */
            def addPoints(userId: ID, pointsToAdd: Point): UserRepositoryFree[Either[String, Unit]] = {
                // Free.flatMap　取出　FindUser(id)
                findUser(userId).flatMap {
                    case None =>
                        Free.pure(Left("User not found"))              // 装箱回去 Free
                    case Some(user) =>
                        val updated = user.copyPoint(pointsToAdd)
                        for {
                            _ <- updateUser(updated) //.left
                            /** sendEmail 的返回类型要和 updateUser 对齐：*/
                            _ <- sendEmail(user.email, "Points added!", s"You now have ${updated.loyaltyPoints}") //.right
                        } yield Right(())
                }
            }

            // Type class for User.copy
            trait UserOps {
                def copyPoint(p: Point): User
                def email:String
                def loyaltyPoints:Point
            }

            implicit def CopyUser(s: User): UserOps
        }

        object implement {
            import cats.{~>}

            // 具体化类型
            object Design extends design {
                type Point = Int
                type ID = UUID
                case class USER(id: ID, email: String, loyaltyPoints: Point)
                type User = USER

                override implicit def CopyUser(s: USER): Design.UserOps = new UserOps {
                    override def copyPoint(p: Int): USER = s.copy(loyaltyPoints = s.loyaltyPoints + p)
                    override def email: String = s.email
                    override def loyaltyPoints: Int = s.loyaltyPoints
                }
            }

            import Design._

            /**
              * 实现业务逻辑
              * */
            // Free router + interpreter (返回容器是 Future)
            val interpreter: UserRepository ~> Future = new (UserRepository ~> Future) {
                override def apply[A](fa: UserRepository[A]): Future[A] = fa match {
                    case FindUser(id) =>
                        Future.successful(None).asInstanceOf[Future[A]]
                    case UpdateUser(u) =>
                        Future.successful(()).asInstanceOf[Future[A]]
                }
            }
        }

        /** 使用 */
        object Client extends App {
            import cats.implicits._
            import implement._
            import Design._

            val result: Future[Either[String, Unit]] =
                /**
                  * addPoints 返回 Free　然后交给　interpreter　去执行业务解释．
                  *
                  * 呼叫过程： Free -> Interpreter
                  * */
                addPoints(UUID.randomUUID(), 10).foldMap(interpreter)

            println(Await.result(result, Duration.Inf))
        }
    }

    /**
      * Tagless version
      *
      * 设计时不定义 Coproduct, 而是由实现时给出，但是没有 Free 支持
      * */
    package TaglessVersion {
        import cats._
        import cats.implicits._

        trait design {
            type ID
            type User
            type Point

            /**
              * Tagless for UserRepository
              *
              * Coproduct 在实现时提供
              * */
            trait UserRepository[F[_]] {
                def findUser(id: ID): F[Option[User]]
                def updateUser(u: User): F[Unit]
            }

            /**
              * Tagless for Email
              * */
            trait EmailService[F[_]] {
                def sendEmail(email: String, subject: String, body: String): F[Unit]
            }

            /** Composite (DSL) */
            def addPoints[F[_]: Monad](userId: ID, pointsToAdd: Point)
                                      /** 实现时为参数 UserRepository 和 EmailService 提供相同的容器 F */
                                      (implicit ur: UserRepository[F], es:EmailService[F]): F[Either[String, Unit]] = {
                ur.findUser(userId).flatMap {
                    case None =>
                        implicitly[Monad[F]].pure(Left("User not found"))
                    case Some(user) =>
                        val updated = user.copyPoint(pointsToAdd)
                        for {
                            _ <- ur.updateUser(updated)
                            /** 设计时不必考虑类型对齐 */
                            _ <- es.sendEmail(user.email, "Points added!",
                                s"You now have ${updated.loyaltyPoints}")
                        } yield Right(())
                }
            }

            // Type class for User.copy
            trait UserOps {
                def copyPoint(p: Point): User
                def email:String
                def loyaltyPoints:Point
            }

            implicit def CopyUser(s: User): UserOps
        }

        object implement {
            object Design extends design{
                type Point = Int
                type ID = UUID
                case class USER(id: ID, email: String, loyaltyPoints: Point)
                type User = USER

                override implicit def CopyUser(s: USER): Design.UserOps = new UserOps {
                    override def copyPoint(p: Int): USER = s.copy(loyaltyPoints = s.loyaltyPoints + p)
                    override def email: String = s.email
                    override def loyaltyPoints: Int = s.loyaltyPoints
                }
            }
            import Design._

            /** Implement tagless */
            import scala.concurrent._

            /**
              * 实现业务逻辑
              *
              * 为　addPoints　提供 UserRepository 和 EmailService 参数
              * */
            implicit val userRepo = new UserRepository[Future] {
                override def findUser(id: UUID): Future[Option[USER]] = Future.successful(None)
                override def updateUser(u: USER): Future[Unit] = Future.successful(())
            }

            implicit val emailService = new EmailService[Future] {
                override def sendEmail(email: String, subject: String, body: String): Future[Unit] = Future.successful{
                    s"Sending email for $email with subject: $subject"
                }
            }
        }

        /** 使用 */
        object Client extends App {
            import implement._
            import Design._

            import cats.instances.future._
            import scala.concurrent.ExecutionContext.Implicits.global

            val result: Future[Either[String, Unit]] =
                /**
                  * 呼叫过程：FutureMonad -> (implicit)UserRepository
                  * */
                addPoints[Future](UUID.randomUUID(), 10)

            println(Await.result(result, Duration.Inf))
        }
    }

    /**
      * Free + Tagless 的版本，同时获得 Tagless 和 Free 的优点.
      * */
    package FreeAndTagless {

    }
}
