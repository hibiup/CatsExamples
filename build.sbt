val catsVersion = "2.7.0"
val catsEffectVersion = "3.3.0"
val catsMtlVersion = "0.7.1"
val scalaTestVersion = "3.2.10"
val specs2Version = "4.3.5"
val logBackVersion = "1.2.8"
val scalaLogging = "3.9.2"
val catsTaglessVersion = "0.14.0"

lazy val CatsExamples = (project in file(".")).
        settings(
            organization := "com.example",
            version := "0.1",
            name := "CatsExamples",
            scalaVersion := "2.13.7",
            /*resolvers ++= Seq(
                "Typesafe backup repo" at "http://repo.typesafe.com/typesafe/repo/",
                // "Maven central" at "http://central.maven.org/maven2"
            ),*/
            libraryDependencies ++= Seq(
                "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
                //"org.specs2" %% "specs2-core" % specs2Version % Test,
                //"org.specs2" %% "specs2-scalacheck" % specs2Version % Test,
                //"org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
                "org.typelevel" %% "cats-core" % catsVersion,
                "org.typelevel" %% "cats-free" % catsVersion,
                "org.typelevel" %% "cats-effect" % catsEffectVersion,
                "org.typelevel" %% "cats-tagless-macros" % catsTaglessVersion,
                //"org.typelevel" %% "cats-tagless-legacy-macros" % catsTaglessVersion,
                "org.typelevel" %% "cats-mtl-core" % catsMtlVersion,
                "com.typesafe" % "config" % "1.3.1",
                "ch.qos.logback" % "logback-classic" % logBackVersion,
                "com.typesafe.scala-logging" %% "scala-logging" % scalaLogging
            ),
            //addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
            //addCompilerPlugin("org.scalameta" %% "paradise" % "2.1.1" cross CrossVersion.full),
            //addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.13.2"),
            scalacOptions ++= Seq(
                //"-Xplugin-require:macroparadise",
                "-language:higherKinds",
                "-deprecation",
                "-encoding", "UTF-8",
                //"-Ypartial-unification",
                "-feature",
                "-language:_"
            )
        )