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
  def layOutSetItemToSVG(diagram: LibRRD.DiagramParameters,
                         nonterminalTargets: js.Map[String, String]): SVGSVGElement =
    LibRRD.layOutSetItemToSVG(diagram, nonterminalTargets.toMap)

  @JSExport
  def layOutSetToSVG(diagrams: js.Array[LibRRD.DiagramParameters],
                     nonterminalTargets: js.Map[String, String])
      : js.Array[SVGSVGElement] =
    LibRRD.layOutSetToSVG(diagrams.toSeq, nonterminalTargets.toMap).toJSArray

  @JSExport
  def replaceByID(diagram: Diagram,
                  id: String,
                  replacement: Diagram): Diagram =
    LibRRD.replaceByID(diagram, id, replacement)

  @JSExport
  def replaceByIDWithLabel(diagram: Diagram,
                           id: String,
                           replacement: Diagram,
                           label: String): Diagram =
    LibRRD.replaceByID(diagram, id, replacement.withMeta(groupLabel = Some(label)))

  @JSExport
  def withSerialIDs(diagram: Diagram, prefix: String): Diagram =
    LibRRD.withSerialIDs(diagram, prefix)
