package ui

object CLI:
  @main def main =
    librrd.LibRRDFile.layOutToSVGFile(
      DiagramParser(GUIPresets.diagramPresets("SQLite table-constraint")).get,
      StylesheetParser(GUIPresets.layoutPresets("SQLite")).get,
      GUIPresets.renderingPresets("SQLite"),
      500,
      "trial.svg")
