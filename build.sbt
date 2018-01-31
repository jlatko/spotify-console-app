name := "spotify-console-app"

version := "0.1"

scalaVersion := "2.11.8"


resolvers += "jitpack" at "https://jitpack.io"
libraryDependencies += "com.github.Jakeway" % "spotify-web-api-scala" % "-SNAPSHOT"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies +=  "se.michaelthelin.spotify" % "spotify-web-api-java" % "1.5.0"
