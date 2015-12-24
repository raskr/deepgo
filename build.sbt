name := "deepgo"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies  ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.xerial" % "sqlite-jdbc" % "3.8.11.2",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)
showSuccess := false
outputStrategy := Some(StdoutOutput)


Project.defaultSettings ++ Seq(
  fork                  :=   true, // Fork to separate process
  connectInput in run   :=   true, // Connects stdin to sbt during forked runs
  outputStrategy        :=   Some(StdoutOutput) // Get rid of output prefix
)