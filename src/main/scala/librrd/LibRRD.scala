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

  def normalizeID(name: String): String = name.replaceAll(" ", "-")

  def layOutSetItemToSVG(params: DiagramParameters, topLevelID: Boolean = true): SVGSVGElement =
    import LayoutStylesheets.*

    val topLevelLabelRule = Rule(List(Root),
      List(Property(LabelPosition,
        librrd.LabelPositionValue(librrd.LabelPositionBlock.Top, librrd.LabelPositionInline.Left))))
    def topLevelLabelCSS =
      val elem = createElementNS("http://www.w3.org/2000/svg", "style")
      elem.textContent = "svg rect.librrd-group:has(+ g > .top-level) { fill: none; stroke: none; }"
      elem

    val normalizedName = normalizeID(params.name)
    val diagram = params.diagram.withMeta(
      classes = params.diagram.classes + "top-level",
      id = if topLevelID then Some(normalizedName) else None,
      groupLabel = Some(params.name))
    val svg = layOutToSVG(
      diagram,
      Stylesheet(topLevelLabelRule +: params.stylesheet.rules),
      params.width)
    svg.appendChild(topLevelLabelCSS)
    svg.querySelectorAll(".librrd-nonterminal:not(.librrd-nolink) > text").foreach { e =>
      val link = createElementNS("http://www.w3.org/2000/svg", "a")
      link.setAttribute("href", s"#${normalizeID(e.textContent)}")
      link.setAttribute("target", "_top")
      e.replaceWith(link)
      link.appendChild(e)
    }
    svg

  def layOutSetToSVG(diagrams: Seq[DiagramParameters], topLevelID: Boolean = true)
      : Seq[SVGSVGElement] =
    diagrams.map(params => layOutSetItemToSVG(params, topLevelID))

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
