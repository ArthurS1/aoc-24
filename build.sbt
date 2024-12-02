ThisBuild / scalaVersion := "2.13.15"

lazy val exercices =  (project in file("."))
  .settings(
    name := "Exercises",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0"
  )

