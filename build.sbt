name := "sort"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += "peoplepattern" at "https://dl.bintray.com/peoplepattern/maven/"

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-native" % "3.3.0",
  "com.peoplepattern" %% "lib-text" % "0.3"
)