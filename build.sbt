val catsVersion = "1.6.0"
val catsEffectVersion = "1.2.0"
val scalaTestVersion = "3.0.5"
val specs2Version = "4.3.5"
val logBackVersion = "1.2.3"
val scalaLogging = "3.9.2"
val catsTaglessVersion = "0.5"

lazy val CatsExamples = (project in file(".")).
        settings(
            organization := "com.example",
            version := "0.1",
            name := "CatsExamples",
            scalaVersion := "2.12.7",
            libraryDependencies ++= Seq(
                "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
                //"org.specs2" %% "specs2-core" % specs2Version % Test,
                //"org.specs2" %% "specs2-scalacheck" % specs2Version % Test,
                //"org.scalacheck" %% "scalacheck" % "1.14.0", Test,
                "org.typelevel" %% "cats-core" % catsVersion,
                "org.typelevel" %% "cats-free" % catsVersion,
                "org.typelevel" %% "cats-effect" % catsEffectVersion,
                "org.typelevel" %% "cats-tagless-macros" % catsTaglessVersion,
                "org.typelevel" %% "cats-tagless-legacy-macros" % catsTaglessVersion,
                "org.typelevel" %% "cats-mtl-core" % "0.2.1",
                "ch.qos.logback" % "logback-classic" % logBackVersion,
                "com.typesafe.scala-logging" %% "scala-logging" % scalaLogging
            ),
            addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.full),
            addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
            scalacOptions ++= Seq(
                "-Xplugin-require:macroparadise",
                "-language:higherKinds",
                "-deprecation",
                "-encoding", "UTF-8",
                "-Ypartial-unification",
                "-feature",
                "-language:_"
            )
        )