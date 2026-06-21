package librrd

import org.scalajs.dom.SVGSVGElement
import org.scalajs.dom.document.createElementNS

object LibRRD:

  def preLayOut(diagram: Diagrams.Diagram, stylesheet: LayoutStylesheets.Stylesheet) =
    AlignedDiagrams.align(
      DirectedDiagrams.direct(
        ParameterizedDiagrams.parameterize(diagram, stylesheet)))

  val SVGWD = WrappedDiagrams(LayoutsSVG)

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

  def layOutSetToSVG(
      diagrams: Seq[DiagramParameters],
      commonStylesheet: LayoutStylesheets.Stylesheet,
      topLevelID: Boolean = true): Seq[SVGSVGElement] =
    import LayoutStylesheets.*

    def normalizeName(name: String): String = name.replaceAll(" ", "-")
    val topLevelLabelRule = Rule(List(Root),
      List(Property(LabelPosition,
        librrd.LabelPositionValue(librrd.LabelPositionBlock.Top, librrd.LabelPositionInline.Left))))
    def topLevelLabelCSS =
      val elem = createElementNS("http://www.w3.org/2000/svg", "style")
      elem.textContent = "svg rect.librrd-group:has(+ g > .top-level) { fill: none; stroke: none; }"
      elem

    diagrams.map(params =>
      val normalizedName = normalizeName(params.name)
      // TODO gross duplication but how else to do it?
      val diagram = params.diagram match
        case d: Diagrams.TerminalToken => d.copy(
          groupLabel = Some(params.name),
          id = if topLevelID then Some(normalizedName) else None,
          classes = d.classes + "top-level")
        case d: Diagrams.NonterminalToken => d.copy(
          groupLabel = Some(params.name),
          id = if topLevelID then Some(normalizedName) else None,
          classes = d.classes + "top-level")
        case d: Diagrams.Sequence => d.copy(
          groupLabel = Some(params.name),
          id = if topLevelID then Some(normalizedName) else None,
          classes = d.classes + "top-level")
        case d: Diagrams.Stack => d.copy(
          groupLabel = Some(params.name),
          id = if topLevelID then Some(normalizedName) else None,
          classes = d.classes + "top-level")
      val svg = layOutToSVG(
        diagram,
        Stylesheet((topLevelLabelRule +: commonStylesheet.rules) ++ params.stylesheet.rules),
        params.width)
      svg.appendChild(topLevelLabelCSS)
      svg.querySelectorAll(".librrd-nonterminal > text").foreach{ e =>
        val link = createElementNS("http://www.w3.org/2000/svg", "a")
        link.setAttribute("href", s"#${normalizeName(e.textContent)}")
        link.setAttribute("target", "_top")
        e.replaceWith(link)
        link.appendChild(e)
      }
      svg)
