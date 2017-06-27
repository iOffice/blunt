import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.iofficecorp",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "blunt",
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-core-cats" % "0.4.1",
      "org.tpolecat" %% "doobie-scalatest-cats" % "0.4.1" % Test,
      "com.h2database" % "h2" % "1.4.196" % Test,
      "com.chuusai" %% "shapeless" % "2.3.2",
      "org.atteo" % "evo-inflector" % "1.2.2",
      scalaTest % Test
    ),
    scalacOptions ++= Seq(
      "-feature"
    )
  )
