scalaVersion in Global := "2.11.2"

scalacOptions in Global := Seq(
  "-feature",
  "-deprecation",
  "-encoding", "utf8",
  "-language:postfixOps",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-target:jvm-1.7",
  "-unchecked",
  "-Xcheckinit",
  "-Xfuture",
  "-Xlint",
  "-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard")

lazy val funlib = project.in(file(".")).aggregate(core,data)

lazy val core = project

lazy val data = project.dependsOn(std)

lazy val std = project.dependsOn(core)
