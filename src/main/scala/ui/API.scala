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
  def minMaxContent(diagram: Diagram, stylesheet: Stylesheet): js.Object =
    val m = LibRRD.minMaxContent(diagram, stylesheet)
    new js.Object {
      val minContent = m._1
      val maxContent = m._2
    }

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
  def layOutSetItemToSVG(diagram: LibRRD.DiagramParameters, topLevelID: Boolean): SVGSVGElement =
    LibRRD.layOutSetItemToSVG(diagram, topLevelID)

  @JSExport
  def layOutSetToSVG(diagrams: js.Array[LibRRD.DiagramParameters], topLevelID: Boolean)
      : js.Array[SVGSVGElement] =
    LibRRD.layOutSetToSVG(diagrams.toSeq, topLevelID).toJSArray

  @JSExport
  def inlineNonterminal(diagram: Diagram,
                        nonterminal: String,
                        replacement: Diagram): Diagram =
    LibRRD.inlineNonterminal(diagram, nonterminal, replacement)
