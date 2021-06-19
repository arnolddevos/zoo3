lazy val root = project 
  .in(file("."))
  .aggregate(insitu, qeduce, summit, minio)
  .settings( scalaVersion := SV )

def SV = "3.0.0"

def simpleProject(x: String, v: String) =
  Project(x, file(x)).settings(
    name := x,
    version := v,
    scalaVersion := SV
  )

lazy val insitu = simpleProject("insitu", "0.1")
lazy val qeduce = simpleProject("qeduce", "0.1").dependsOn(summit)
lazy val summit = simpleProject("summit", "0.1")
lazy val minio = simpleProject("minio", "0.1").dependsOn(insitu)
