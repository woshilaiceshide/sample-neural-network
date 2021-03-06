organization := "woshilaiceshide"

name := "sample-neural-network"

version := "1.0"

description := ""

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

publishMavenStyle := true

enablePlugins(BintrayPlugin)

pomIncludeRepository  := {_ => false}

bintrayRepository := "maven"

bintrayOrganization := None

bintrayVcsUrl := Some(s"git@github.com:woshilaiceshide/${name.value}.git")

bintrayReleaseOnPublish in ThisBuild := false

compileOrder in Compile := CompileOrder.Mixed

transitiveClassifiers := Seq("sources")

EclipseKeys.withSource := true

scalaVersion := "2.11.8"

scalacOptions := Seq("-unchecked", "-deprecation","-optimise", "-encoding", "utf8", "-Yno-adapted-args")

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked", "-source", "1.7", "-target", "1.7", "-g:vars")

libraryDependencies += "com.typesafe.play" %% "anorm" % "2.5.1"

libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.8.11.2"

libraryDependencies += "com.zaxxer" % "HikariCP" % "2.4.7"

libraryDependencies += "com.jolbox" % "bonecp" % "0.8.0.RELEASE"

//libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"

retrieveManaged := false

enablePlugins(JavaAppPackaging)

unmanagedSourceDirectories in Compile <+= baseDirectory( _ / "src" / "java" )

unmanagedSourceDirectories in Compile <+= baseDirectory( _ / "src" / "scala" )
