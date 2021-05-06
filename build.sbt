lazy val root = (project in file("."))
  .aggregate(insitu, qeduce, summit, minio)

lazy val insitu = (project in file("insitu"))
lazy val qeduce = (project in file("qeduce")).dependsOn(summit)
lazy val summit = (project in file("summit"))
lazy val minio = (project in file("minio"))
