
def SV = "3.0.0"
def V  = "0.6.1"
def O  = "com.backgroundsignal"

lazy val root = 
  Project("zoo", file("."))
  .settings( 
    organization := O,
    version := V,
    scalaVersion := SV 
  )
  .aggregate(insitu, qeduce, summit, minio)
  .dependsOn(insitu, qeduce, summit, minio)

def subProject(x: String, v: String) =
  Project(x, file(x)).settings(
    organization := O,
    version := v,
    scalaVersion := SV
  )

lazy val insitu = subProject("insitu", V)
lazy val qeduce = subProject("qeduce", V)
  .dependsOn(summit)
  .settings(libraryDependencies += "com.lihaoyi" %% "geny" % "0.6.10")
lazy val summit = subProject("summit", V)
lazy val minio  = subProject("minio",  V).dependsOn(insitu)
