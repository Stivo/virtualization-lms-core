name := "LMS"

version := "0.1"

organization := "EPFL"

resolvers += ScalaToolsSnapshots

resolvers += dropboxScalaTestRepo

scalaVersion := virtScala

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

scalacOptions += "-Yvirtualize" 

//scalacOptions in Compile ++= Seq(/*Unchecked, */Deprecation)


// needed for scala.tools, which is apparently not included in sbt's built in version
libraryDependencies += "org.scala-lang" % "scala-library" % virtScala

libraryDependencies += "org.scala-lang" % "scala-compiler" % virtScala

libraryDependencies += scalaTest

parallelExecution in Test := false


// continuations
autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % virtScala)

scalacOptions += "-P:continuations:enable"
