package librrd

object LibRRD:

  def preLayOut(diagram: Diagrams.Diagram, stylesheet: LayoutStylesheets.Stylesheet) =
    AlignedDiagrams.align(
      DirectedDiagrams.direct(
        ParameterizedDiagrams.parameterize(diagram, stylesheet)))

  val SVGWD = WrappedDiagrams(LayoutsSVG)

  def layOutToSVG(diagram: Diagrams.Diagram, stylesheet: LayoutStylesheets.Stylesheet,
                  width: Double) =
    val myWrappedDiagram = SVGWD.wrapLocally(preLayOut(diagram, stylesheet))
    val myWidth = Math.max(if width <= 1 then width * myWrappedDiagram.maxContent else width,
                           myWrappedDiagram.minContent)
    val myLayout = JustifiedDiagrams.justify(SVGWD)(myWrappedDiagram, myWidth)
    LayoutsSVG.renderToSVG(SVGWD.backend.render(myLayout))

  def layOutGloballyToSVG(diagram: Diagrams.Diagram, stylesheet: LayoutStylesheets.Stylesheet,
                        width: Double) =
    val myWrappedDiagram = SVGWD.wrapGlobally(preLayOut(diagram, stylesheet))
    val myWidth = Math.max(if width <= 1 then width * myWrappedDiagram.maxContent else width,
                           myWrappedDiagram.minContent)
    val myLayout = JustifiedDiagrams.justify(SVGWD)(myWrappedDiagram, myWidth)
    LayoutsSVG.renderToSVG(SVGWD.backend.render(myLayout))
