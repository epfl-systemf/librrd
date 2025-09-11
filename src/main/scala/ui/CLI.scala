package ui

import scalajs.js
import js.annotation.*

object CLI:

  @js.native
  @JSImport("process", "argv")
  val argv: js.Array[String] = js.native

  @main def main =
    println(argv.toList.drop(2))
    librrd.LibRRDFile.layOutToSVGFile(
      DiagramParser(GUIPresets.diagramPresets("SQLite table-constraint")).get,
      StylesheetParser(GUIPresets.layoutPresets("SQLite")).get,
      GUIPresets.renderingPresets("SQLite"),
      500,
      "trial.svg")
