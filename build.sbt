import AssemblyKeys._

assemblySettings

name := "nextbus"

version := "0.1"

scalaVersion := "2.10.1"

resolvers += "spray" at "http://repo.spray.io/"

libraryDependencies += "io.spray" %%  "spray-json" % "1.2.5"
