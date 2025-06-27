enablePlugins(ScalaJSPlugin)

ThisBuild / scalaVersion := "3.3.6"
val deploy = taskKey[Unit]("copy JS modules to www")

lazy val root = project.in(file("."))
  .settings(
    Global / onChangedBuildSource := ReloadOnSourceChanges,
    name := "librrd",
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },

    libraryDependencies ++= Seq(
      "org.creativescala" %%% "doodle" % "0.30.0",
      "com.lihaoyi" %%% "scalatags" % "0.11.0",
    ),

    Compile / deploy := {
      import java.nio.file.*
      val targetPath = target.value.toPath()
        .resolve("scala-" + scalaVersion.value)
        .resolve(name.value + "-fastopt")
      val output = (Compile / fastLinkJS).value
      output.data.publicModules.foreach { module =>
        Files.copy(
          targetPath.resolve(module.jsFileName),
          Paths.get("www", module.jsFileName),
          StandardCopyOption.REPLACE_EXISTING)
        module.sourceMapName.foreach(n => Files.copy(
          targetPath.resolve(n),
          Paths.get("www", n),
          StandardCopyOption.REPLACE_EXISTING))
      }
    }
  )
