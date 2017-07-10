import Dependencies._

val artifactoryUserName = sys.props.get("iOfficeBartifactoryUsername")
val artifactoryPassword = sys.props.get("iOfficeBartifactoryPassword")
val artifactoryHost = "http://bartifactory.corp.iofficecorp.com:8081"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.iofficecorp",
      scalaVersion := "2.12.2",
      crossScalaVersions := Seq("2.12.2", "2.11.8"),
      version      := "0.1.0"
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
    ),
    publishTo := Some("Artifactory Realm" at s"$artifactoryHost/artifactory/libs-release-local"),
    credentials += Credentials(
      "Artifactory Realm",
      "localhost",
      artifactoryUserName.getOrElse("noUsernameSet"),
      artifactoryPassword.getOrElse("noPasswordSet")
    )
  )
