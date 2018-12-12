name := "CatsExamples"

version := "0.1"

scalaVersion := "2.12.7"

val catsVersion = "1.5.0"
val scalaTestVersion = "3.0.5"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
    "org.typelevel" %% "cats-core" % catsVersion,
    //"org.typelevel" %% "cats-free" % catsVersion,
    //"org.typelevel" %% "cats-tagless-macros" % "0.2.0",
    "org.typelevel" %% "cats-mtl-core" % "0.4.0"
)
