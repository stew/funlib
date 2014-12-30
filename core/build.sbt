name := "funlib-core"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck"                % "1.12.+" % "test",
  "org.specs2"     %% "specs2-core"               % "2.4.+"  % "test",
  "org.typelevel"  %% "scalaz-specs2"             % "0.3.+"  % "test",
  "org.scalaz"     %% "scalaz-core"               % "7.1.+"  % "test",
    "org.scalaz"   %% "scalaz-scalacheck-binding" % "7.1.+"  % "test"
)

