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
    val myLayout = JustifiedDiagrams.justify(SVGWD)(myWrappedDiagram, width)
    SVGWD.backend.render(myLayout)

  def resetSVGID() = LayoutsSVG.resetID()
