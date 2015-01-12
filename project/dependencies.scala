import sbt._
import sbt.Keys._

object Dependencies {
  // Versions
  val liftVersion = "2.6"
  val servletVersion = "3.0.1"
  val logbackVersion = "1.1.2"
    
  // Libraries
  val liftWebkit = "net.liftweb" %% "lift-webkit" % liftVersion
  val servlet = "javax.servlet" % "javax.servlet-api" % servletVersion
  val logback = "ch.qos.logback" % "logback-classic" % logbackVersion
  
  // Projects
  val lift_validationDependencies = Seq(liftWebkit, logback, servlet % "provided")
}