name := "spotify-console-app"

version := "0.1"

scalaVersion := "2.11.8"


resolvers ++= Seq(
  "jitpack" at "https://jitpack.io",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)
libraryDependencies += "com.github.Jakeway" % "spotify-web-api-scala" % "-SNAPSHOT"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies +=  "se.michaelthelin.spotify" % "spotify-web-api-java" % "1.5.0"
libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "0.13.2",
  "org.scalanlp" %% "breeze-natives" % "0.13.2",
  "org.scalanlp" %% "breeze-viz" % "0.13.2"
)

// sbt-assembly
mainClass in assembly := Some("MyApp")
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}