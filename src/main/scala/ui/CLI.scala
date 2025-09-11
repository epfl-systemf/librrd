package ui

@scalajs.js.annotation.JSExportTopLevel("CLI")
object CLI:
  def main =
    librrd.LibRRD.layOutToSVGFile(
      DiagramParser(GUIPresets.diagramPresets("SQLite table-constraint")).get,
      StylesheetParser(GUIPresets.layoutPresets("SQLite")).get,
      GUIPresets.renderingPresets("SQLite"),
      500,
      "trial.svg")
