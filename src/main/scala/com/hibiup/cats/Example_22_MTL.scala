package com.hibiup.cats

package Example_22_MTL {
    /**
      * Cats MTL(Monad Transformer Library) 是 Cats 的容器转换库
      * */
    object TestMTL extends App {
        import com.typesafe.config.ConfigFactory

        import cats.effect.IO
        import cats.Monad
        import cats.effect.LiftIO
        import cats.mtl.ApplicativeAsk
        import cats.implicits._
        import cats.data.Kleisli

        type Config = String
        type Result = Boolean

        def getConfig: IO[Config] = IO{ConfigFactory.parseString(
            """
              | number.value : 100
            """.stripMargin).getString("number.value")}

        def checkDifference(c:Config):Result = (110-100).abs > c.toInt

        // F[_] == Kleisli[IO, Config, _]
        // F[_]: Monad: LiftIO == LiftIO[Monad[F[_]]]
        def readerProgram[F[_]: Monad: LiftIO](implicit A: ApplicativeAsk[F, Config]): F[Result] = for {
            config <- A.ask
            result <- IO.pure( checkDifference(config) ).to[F]  // IO to Kleisli[IO, Config, ?]
        } yield result

        import cats.mtl.implicits._
        val meterializeProgram = readerProgram[Kleisli[IO, Config, ?]]  // type F == Kleisli[IO, Config, ?]

        println(getConfig.flatMap(meterializeProgram.run).unsafeRunSync())   // False
    }
}
