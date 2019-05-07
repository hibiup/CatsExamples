package com.hibiup.cats

package Example_22_MTL {

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
        // F[_]: Monad: LiftIO == LiftIO[Monad[F[_]]]
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

        type L[A] = Either[Throwable, A]
        implicit def LiftIO2Either: LiftIO[L] = new LiftIO[L] {
            override def liftIO[A](ioa: IO[A]): L[A] = ioa.attempt.unsafeRunSync()
        }

        implicitly[LiftIO[Either[Throwable, ?]]].liftIO(compare(ConfigFactory.parseString(
            """
              | number.value : 100
            """.stripMargin))) match {
            case Right(r) => println(r)
            case Left(t) => println(t.getMessage)
        }
    }


}
