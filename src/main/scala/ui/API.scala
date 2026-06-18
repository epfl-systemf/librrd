package ui

import scalajs.js
import js.annotation.*
import org.scalajs.dom.SVGSVGElement
import librrd.Diagrams.Diagram
import librrd.LayoutStylesheets.Stylesheet
import librrd.LibRRD

@scalajs.js.annotation.JSExportTopLevel("LibRRD", "librrd")
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
