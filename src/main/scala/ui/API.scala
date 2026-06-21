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

  @JSExportTopLevel("DiagramParameters", "librrd")
  class DiagramParameters(
      val name: String,
      val diagram: Diagram,
      val stylesheet: Stylesheet,
      val width: Double) extends js.Object:
    def this(name: String, diagram: String, stylesheet: String, width: Double) =
      this(name, parseDiagram(diagram), parseStylesheet(stylesheet), width)
    def _toLibRRDParams = LibRRD.DiagramParameters(name, diagram, stylesheet, width)

  @JSExport
  def layOutSetToSVG(diagrams: js.Array[DiagramParameters], commonStylesheet: Stylesheet)
      : js.Array[SVGSVGElement] =
    LibRRD.layOutSetToSVG(diagrams.toSeq.map(_._toLibRRDParams), commonStylesheet).toJSArray
  @JSExport
  def layOutSetToSVG(diagrams: js.Array[DiagramParameters], commonStylesheet: String)
      : js.Array[SVGSVGElement] =
    LibRRD.layOutSetToSVG(diagrams.toSeq.map(_._toLibRRDParams),
      parseStylesheet(commonStylesheet)).toJSArray
