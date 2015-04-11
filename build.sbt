version := "0.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "tests")

retrieveManaged := true

