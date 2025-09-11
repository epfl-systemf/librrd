enablePlugins(ScalaJSPlugin)

import sbt.Keys.streams
import org.scalajs.linker.interface.ModuleInitializer

ThisBuild / scalaVersion := "3.3.6"
val deploy = taskKey[Unit]("copy JS modules to www")

lazy val root = project.in(file("."))
  .settings(
    Global / onChangedBuildSource := ReloadOnSourceChanges,

    name := "librrd",
    Compile / scalaJSModuleInitializers ++= Seq(
      ModuleInitializer.mainMethod("ui.GUI", "main").withModuleID("gui"),
      ModuleInitializer.mainMethod("ui.CLI", "main").withModuleID("cli")
    ),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    scalacOptions ++= Seq(
      s"-scalajs-mapSourceURI:file://${baseDirectory.value}->/",
      "-Ysafe-init",
      "-Wunused:all",
      "-Wvalue-discard",
      "-deprecation",
    ),

    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "com.lihaoyi" %%% "scalatags" % "0.13.1",
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0",
      "org.rogach" %%% "scallop" % "5.2.0",
    ),

    Compile / deploy := {
      streams.value.log.info("copying output js files and sourcemaps…")
      val output = (Compile / fastLinkJS).value
      import java.nio.file.*
      val targetPath = target.value.toPath()
        .resolve("scala-" + scalaVersion.value)
        .resolve(name.value + "-fastopt")
      def deployModule(id: String) =
        targetPath.toFile().listFiles()
          .map(_.toPath())
          .filter { p =>
            val n = p.getFileName().toString()
            n.startsWith(id) || n.startsWith("internal") }
          .foreach(p => Files.copy(p, Paths.get(id, p.getFileName().toString()),
            StandardCopyOption.REPLACE_EXISTING))
      deployModule("gui")
      deployModule("cli")
    }
  )
