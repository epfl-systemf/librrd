package librrd

import scalajs.js
import scalajs.js.annotation.*

@JSExportTopLevel("LibRRDFile", "cli")
object LibRRDFile:

  @js.native
  @JSImport("console", "time")
  def consoleTime(label: String): Unit = js.native
  @js.native
  @JSImport("console", "timeEnd")
  def consoleTimeEnd(label: String): Unit = js.native

  val SVGFileWD = WrappedDiagrams(SBlockLayoutsSVGFile)

  def layOutToSVGFile(
      diagram: Diagrams.Diagram,
      layoutStylesheet: LayoutStylesheets.Stylesheet,
      renderingStylesheet: String,
      width: Double,
      filename: String,
      time: Boolean = false) =
    if time then consoleTime("layout")
    val myWrappedDiagram = SVGFileWD.wrapLocally(LibRRD.preLayOut(diagram, layoutStylesheet))
    val myWidth = Math.max(width * (if width <= 1 then myWrappedDiagram.maxContent else 1),
                           myWrappedDiagram.minContent)
    val myLayout = JustifiedDiagrams.justify(SVGFileWD)(myWrappedDiagram, myWidth)
    if time then consoleTimeEnd("layout")
    SBlockLayoutsSVGFile.renderToFile(
      SVGFileWD.backend.render(myLayout).render, renderingStylesheet,
      myLayout.width, myLayout.height, filename)
