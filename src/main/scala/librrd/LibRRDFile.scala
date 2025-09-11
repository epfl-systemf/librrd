package librrd

import scalajs.js.annotation.*

@JSExportTopLevel("LibRRDFile", "main")
object LibRRDFile:

  val SVGFileWD = WrappedDiagrams(LayoutsSVGFile)

  def layOutToSVGFile(
      diagram: Diagrams.Diagram,
      layoutStylesheet: LayoutStylesheets.Stylesheet,
      renderingStylesheet: String,
      width: Double,
      filename: String) =
    val myWrappedDiagram = SVGFileWD.wrapLocally(LibRRD.preLayOut(diagram, layoutStylesheet))
    val myLayout = JustifiedDiagrams.justify(SVGFileWD)(myWrappedDiagram, width)
    LayoutsSVGFile.renderToFile(SVGFileWD.backend.render(myLayout).render, renderingStylesheet,
                                myLayout.width, myLayout.height, filename)

