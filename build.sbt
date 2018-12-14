val catsVersion = "1.0.1"
val scalaTestVersion = "3.0.5"

lazy val CatsExamples = (project in file(".")).
        settings(
            organization := "com.example",
            version := "0.1",
            name := "CatsExamples",
            scalaVersion := "2.12.7",
            libraryDependencies ++= Seq(
                "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
                "org.typelevel" %% "cats-core" % catsVersion,
                "org.typelevel" %% "cats-free" % catsVersion,
                "org.typelevel" %% "cats-mtl-core" % "0.2.1"
            ),
            addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.full),
            addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
            scalacOptions ++= Seq(
                "-Xplugin-require:macroparadise",
                "-deprecation",
                "-encoding", "UTF-8",
                "-Ypartial-unification",
                "-feature",
                "-language:_"
            )
        )