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

  def inlineNonterminal(diagram: Diagrams.Diagram,
                        nonterminal: String,
                        replacement: Diagrams.Diagram): Diagrams.Diagram =
    diagram match
      case d: Diagrams.TerminalToken => d
      case d @ Diagrams.NonterminalToken(label, _, _, _) =>
        if label == nonterminal then replacement.withMeta(groupLabel = Some(nonterminal)) else d
      case d @ Diagrams.Sequence(subdiagrams, _, _, _) =>
        d.copy(subdiagrams = subdiagrams.map(inlineNonterminal(_, nonterminal, replacement)))
      case d @ Diagrams.Stack(topSubdiagram, bottomSubdiagram, _, _, _, _) =>
        d.copy(topSubdiagram = inlineNonterminal(topSubdiagram, nonterminal, replacement),
               bottomSubdiagram = inlineNonterminal(bottomSubdiagram, nonterminal, replacement))
