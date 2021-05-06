lazy val minio = project
  .in(file("."))
  .settings(
    name := "minio",
    version := "0.1",
    scalaVersion := "3.0.0-RC3"
  )
