name := "CatsExamples"

version := "0.1"

scalaVersion := "2.12.7"

val catsVersion = "1.5.0"
val scalaTestVersion = "3.0.5"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "org.typelevel" %% "cats-core" % catsVersion,
    //"org.typelevel" %% "cats-free" % catsVersion,
    //"org.typelevel" %% "cats-tagless-macros" % "0.2.0",
    "org.typelevel" %% "cats-mtl-core" % "0.4.0"
)

scalacOptions ++= Seq("-Xplugin-require:macroparadise")
addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
