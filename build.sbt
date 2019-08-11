name := "Prolog2Scala"

version := "0.1"

scalaVersion := "2.12.9"

scalacOptions ++= Seq(
  "-language:implicitConversions",
  "-language:postfixOps"
)

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "2.1.3",
  "org.scalactic" %% "scalactic" % "3.0.8",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "com.eed3si9n" %% "treehugger" % "0.4.3"
)