package librrd

import scalajs.js.annotation.*

@JSExportTopLevel("LibRRDFile", "cli")
object LibRRDFile:

  val SVGFileWD = WrappedDiagrams(LayoutsSVGFile)

  def layOutToSVGFile(
      diagram: Diagrams.Diagram,
      layoutStylesheet: LayoutStylesheets.Stylesheet,
      renderingStylesheet: String,
      width: Double,
      filename: String) =
    val myWrappedDiagram = SVGFileWD.wrapLocally(LibRRD.preLayOut(diagram, layoutStylesheet))
    val myWidth = Math.max(width * (if width <= 1 then myWrappedDiagram.maxContent else 1),
                           myWrappedDiagram.minContent)
    val myLayout = JustifiedDiagrams.justify(SVGFileWD)(myWrappedDiagram, myWidth)
    LayoutsSVGFile.renderToFile(SVGFileWD.backend.render(myLayout).render, renderingStylesheet,
                                myLayout.width, myLayout.height, filename)

