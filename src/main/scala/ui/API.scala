package ui

import scalajs.js
import js.annotation.*
import org.scalajs.dom.{SVGSVGElement, HTMLAnchorElement}
import org.scalajs.dom.document.createElementNS
import librrd.Diagrams.*
import librrd.LayoutStylesheets
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
  case class DiagramParameters(
    name: String, diagram: Diagram, stylesheet: Stylesheet, width: Double)

  def normalizeName(name: String): String =
    name.replaceAll(" ", "-")
  val topLevelLabelRule = LayoutStylesheets.Rule(
    List(LayoutStylesheets.Root),
    List(LayoutStylesheets.Property(LayoutStylesheets.LabelPosition,
      librrd.LabelPositionValue(librrd.LabelPositionBlock.Top, librrd.LabelPositionInline.Left))))
  def topLevelLabelCSS =
    val elem = createElementNS("http://www.w3.org/2000/svg", "style")
    elem.textContent = "svg rect.librrd-group:has(+ g > .top-level) { fill: none; stroke: none; }"
    elem

  @JSExport
  def layOutDiagrams(commonStylesheet: Stylesheet, diagrams: js.Array[DiagramParameters])
      : js.Array[SVGSVGElement] =
    diagrams.map(params =>
      val normalizedName = normalizeName(params.name)
      // TODO gross duplication but how else to do it?
      val diagram = params.diagram match
        case d: TerminalToken =>
          d.copy(groupLabel = Some(params.name), id = Some(normalizedName), classes = d.classes + "top-level")
        case d: NonterminalToken =>
          d.copy(groupLabel = Some(params.name), id = Some(normalizedName), classes = d.classes + "top-level")
        case d: Sequence =>
          d.copy(groupLabel = Some(params.name), id = Some(normalizedName), classes = d.classes + "top-level")
        case d: Stack =>
          d.copy(groupLabel = Some(params.name), id = Some(normalizedName), classes = d.classes + "top-level")
      val svg = layOutToSVG(
        diagram,
        Stylesheet((topLevelLabelRule +: commonStylesheet.rules) ++ params.stylesheet.rules),
        params.width)
      svg.appendChild(topLevelLabelCSS)
      svg.querySelectorAll(".librrd-nonterminal > text").foreach{ e =>
        val link = createElementNS("http://www.w3.org/2000/svg", "a").asInstanceOf[HTMLAnchorElement]
        link.setAttribute("href", s"#${normalizeName(e.textContent)}")
        link.setAttribute("target", "_top")
        e.replaceWith(link)
        link.appendChild(e)
      }
      svg)
