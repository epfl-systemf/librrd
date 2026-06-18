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

  val SVGFileWD = WrappedDiagrams(LayoutsSVGFile)

  def layOutToSVGFile(
      diagram: Diagrams.Diagram,
      layoutStylesheet: LayoutStylesheets.Stylesheet,
      renderingStylesheet: String,
      width: Double,
      filename: String,
      time: Boolean = false) =
    if time then consoleTime("layout")
    val myWrappedDiagram = SVGFileWD.wrapLocally(LibRRD.preLayOut(diagram, layoutStylesheet))
    val myWidth = Math.max(if width <= 1 then width * myWrappedDiagram.maxContent else width,
                           myWrappedDiagram.minContent)
    val myLayout = JustifiedDiagrams.justify(SVGFileWD)(myWrappedDiagram, myWidth)
    if time then consoleTimeEnd("layout")
    LayoutsSVGFile.renderToFile(SVGFileWD.backend.render(myLayout), renderingStylesheet, filename)

  def layOutGloballyToSVGFile(
      diagram: Diagrams.Diagram,
      layoutStylesheet: LayoutStylesheets.Stylesheet,
      renderingStylesheet: String,
      width: Double,
      filename: String,
      time: Boolean = false) =
    if time then consoleTime("layout")
    val myWrappedDiagram = SVGFileWD.wrapGlobally(LibRRD.preLayOut(diagram, layoutStylesheet))
    val myWidth = Math.max(if width <= 1 then width * myWrappedDiagram.maxContent else width,
                           myWrappedDiagram.minContent)
    val myLayout = JustifiedDiagrams.justify(SVGFileWD)(myWrappedDiagram, myWidth)
    if time then consoleTime("layout")
    LayoutsSVGFile.renderToFile(SVGFileWD.backend.render(myLayout), renderingStylesheet, filename)
