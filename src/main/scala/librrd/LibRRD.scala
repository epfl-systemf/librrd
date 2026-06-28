package librrd

import org.scalajs.dom.SVGSVGElement
import org.scalajs.dom.document.createElementNS

object LibRRD:

  def preLayOut(diagram: Diagrams.Diagram, stylesheet: LayoutStylesheets.Stylesheet) =
    AlignedDiagrams.align(
      DirectedDiagrams.direct(
        ParameterizedDiagrams.parameterize(diagram, stylesheet)))

  val SVGWD = WrappedDiagrams(LayoutsSVG)

  def minMaxContent(diagram: Diagrams.Diagram, stylesheet: LayoutStylesheets.Stylesheet): (Double, Double) =
    val wrapped = SVGWD.wrapLocally(preLayOut(diagram, stylesheet))
    (wrapped.minContent, wrapped.maxContent)

  def layOutToSVG(diagram: Diagrams.Diagram, stylesheet: LayoutStylesheets.Stylesheet,
                  width: Double): SVGSVGElement =
    val myWrappedDiagram = SVGWD.wrapLocally(preLayOut(diagram, stylesheet))
    val myWidth = Math.max(if width <= 1 then width * myWrappedDiagram.maxContent else width,
                           myWrappedDiagram.minContent)
    val myLayout = JustifiedDiagrams.justify(SVGWD)(myWrappedDiagram, myWidth)
    LayoutsSVG.renderToSVG(SVGWD.backend.render(myLayout))

  def layOutGloballyToSVG(diagram: Diagrams.Diagram, stylesheet: LayoutStylesheets.Stylesheet,
                        width: Double): SVGSVGElement =
    val myWrappedDiagram = SVGWD.wrapGlobally(preLayOut(diagram, stylesheet))
    val myWidth = Math.max(if width <= 1 then width * myWrappedDiagram.maxContent else width,
                           myWrappedDiagram.minContent)
    val myLayout = JustifiedDiagrams.justify(SVGWD)(myWrappedDiagram, myWidth)
    LayoutsSVG.renderToSVG(SVGWD.backend.render(myLayout))

  case class DiagramParameters(
    name: String,
    diagram: Diagrams.Diagram,
    stylesheet: LayoutStylesheets.Stylesheet,
    width: Double)

  def layOutSetItemToSVG(params: DiagramParameters,
                         nonterminalTargets: Map[String, String]): SVGSVGElement =
    import LayoutStylesheets.*

    val topLevelLabelRule = Rule(List(Root),
      List(Property(LabelPosition,
        librrd.LabelPositionValue(librrd.LabelPositionBlock.Top, librrd.LabelPositionInline.Left))))
    def topLevelLabelCSS =
      val elem = createElementNS("http://www.w3.org/2000/svg", "style")
      elem.textContent = "svg rect.librrd-group:has(+ g > .top-level) { fill: none; stroke: none; }"
      elem

    val diagram = params.diagram.withMeta(
      classes = params.diagram.classes + "top-level",
      groupLabel = Some(params.name))
    val svg = layOutToSVG(
      diagram,
      Stylesheet(topLevelLabelRule +: params.stylesheet.rules),
      params.width)
    svg.appendChild(topLevelLabelCSS)
    svg.querySelectorAll(".librrd-nonterminal:not(.librrd-nolink) > text").foreach { e =>
      if nonterminalTargets.contains(e.textContent) then
        val link = createElementNS("http://www.w3.org/2000/svg", "a")
        link.setAttribute("href", s"#${nonterminalTargets(e.textContent)}")
        link.setAttribute("target", "_top")
        e.replaceWith(link)
        link.appendChild(e): Unit
    }
    svg

  def layOutSetToSVG(diagrams: Seq[DiagramParameters], nonterminalTargets: Map[String, String])
      : Seq[SVGSVGElement] =
    diagrams.map(params => layOutSetItemToSVG(params, nonterminalTargets))

  def replaceByID(diagram: Diagrams.Diagram, id: String, replacement: Diagrams.Diagram): Diagrams.Diagram =
    diagram match
      case d @ Diagrams.TerminalToken(_, _, thisID, _) =>
        if thisID.exists(_ == id) then replacement else d
      case d @ Diagrams.NonterminalToken(_, _, thisID, _) =>
        if thisID.exists(_ == id) then replacement else d
      case d @ Diagrams.Sequence(subdiagrams, _, thisID, _) =>
        if thisID.exists(_ == id) then replacement
        else d.copy(subdiagrams = subdiagrams.map(replaceByID(_, id, replacement)))
      case d @ Diagrams.Stack(topSubdiagram, bottomSubdiagram, _, _, thisID, _) =>
        if thisID.exists(_ == id) then replacement
        else d.copy(topSubdiagram = replaceByID(topSubdiagram, id, replacement),
                    bottomSubdiagram = replaceByID(bottomSubdiagram, id, replacement))

  def withSerialIDs(diagram: Diagrams.Diagram, prefix: String): Diagrams.Diagram =
    var cur = 0
    def withID(d: Diagrams.Diagram): Diagrams.Diagram =
      cur += 1
      d.withMeta(id = d.id.filterNot(_.startsWith(prefix))
                        .orElse(Some(prefix + cur.toString())))
    def rec(d: Diagrams.Diagram): Diagrams.Diagram =
      d match
        case d: (Diagrams.TerminalToken | Diagrams.NonterminalToken) => withID(d)
        case d @ Diagrams.Sequence(subdiagrams, _, _, _) =>
          withID(d.copy(subdiagrams = subdiagrams.map(rec)))
        case d @ Diagrams.Stack(topSubdiagram, bottomSubdiagram, _, _, _, _) =>
          withID(d.copy(topSubdiagram = rec(topSubdiagram),
                        bottomSubdiagram = rec(bottomSubdiagram)))
    rec(diagram)
