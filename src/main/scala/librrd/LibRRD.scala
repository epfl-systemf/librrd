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

  val SVGFileWD = WrappedDiagrams(LayoutsSVGFile)

  def layOutToSVGFile(
      diagram: Diagrams.Diagram,
      layoutStylesheet: LayoutStylesheets.Stylesheet,
      renderingStylesheet: String,
      width: Double,
      filename: String) =
    val myWrappedDiagram = SVGFileWD.wrapLocally(preLayOut(diagram, layoutStylesheet))
    val myLayout = JustifiedDiagrams.justify(SVGFileWD)(myWrappedDiagram, width)
    LayoutsSVGFile.renderToFile(SVGFileWD.backend.render(myLayout).render, renderingStylesheet,
                                myLayout.width, myLayout.height, filename)

  def layOutGloballyToSVG(diagram: Diagrams.Diagram, stylesheet: LayoutStylesheets.Stylesheet,
                        width: Double) =
    val myWrappedDiagram = SVGWD.wrapGlobally(preLayOut(diagram, stylesheet))
    val myLayout = JustifiedDiagrams.justify(SVGWD)(myWrappedDiagram, width)
    SVGWD.backend.render(myLayout)

  def resetSVGID() = LayoutsSVG.resetID()
