name := "Prolog2Scala"

version := "0.1"

scalaVersion := "2.13.0"

scalacOptions ++= Seq(
  "-language:implicitConversions",
  "-language:postfixOps"
)

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "2.1.3"
)