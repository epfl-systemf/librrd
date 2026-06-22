package ui

import scalajs.js
import js.annotation.*
import js.JSConverters.*
import org.scalajs.dom.SVGSVGElement
import librrd.Diagrams.Diagram
import librrd.LayoutStylesheets.Stylesheet
import librrd.LibRRD

@JSExportTopLevel("LibRRD", "librrd")
object API:

  @JSExport
  def parseDiagram(diagram: String): Diagram =
    DiagramParser(diagram).get

  @JSExport
  def parseStylesheet(stylesheet: String): Stylesheet =
    StylesheetParser(stylesheet).get

  @JSExport
  def layOutToSVG(diagram: Diagram, stylesheet: Stylesheet, width: Double): SVGSVGElement =
    LibRRD.layOutToSVG(diagram, stylesheet, width)
  @JSExport
  def layOutToSVG(diagram: String, stylesheet: String, width: Double): SVGSVGElement =
    layOutToSVG(parseDiagram(diagram), parseStylesheet(stylesheet), width)

  @JSExport
  def makeDiagramParameters(
      name: String,
      diagram: Diagram,
      stylesheet: Stylesheet,
      width: Double): LibRRD.DiagramParameters =
    LibRRD.DiagramParameters(name, diagram, stylesheet, width)
  @JSExport
  def makeDiagramParameters(
      name: String,
      diagram: String,
      stylesheet: String,
      width: Double): LibRRD.DiagramParameters =
    LibRRD.DiagramParameters(name, parseDiagram(diagram), parseStylesheet(stylesheet), width)

  @JSExport
  def normalizeID(id: String): String = LibRRD.normalizeID(id)

  @JSExport
  def layOutSetToSVG(diagrams: js.Array[LibRRD.DiagramParameters], commonStylesheet: Stylesheet,
                     topLevelID: Boolean)
      : js.Array[SVGSVGElement] =
    LibRRD.layOutSetToSVG(diagrams.toSeq, commonStylesheet, topLevelID)
      .toJSArray
  @JSExport
  def layOutSetToSVG(diagrams: js.Array[LibRRD.DiagramParameters], commonStylesheet: String,
                     topLevelID: Boolean)
      : js.Array[SVGSVGElement] =
    LibRRD.layOutSetToSVG(diagrams.toSeq, parseStylesheet(commonStylesheet), topLevelID)
      .toJSArray
