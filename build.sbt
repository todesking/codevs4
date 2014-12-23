name := "codevs4-runner"

organization := "com.todesking"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.2" % "test"
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oFT")

