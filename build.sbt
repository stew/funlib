name := "funlib"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
  "org.typelevel" %% "scalaz-specs2" % "0.1.5" % "test",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.0.5" % "test",
  "org.scalaz" %% "scalaz-core" % "7.0.5",
  "com.github.axel22" %% "scalameter" % "0.4"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
