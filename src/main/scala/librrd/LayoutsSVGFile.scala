package librrd

import scalajs.js
import js.annotation.*
import scalatags.Text
import scalatags.Text.implicits.*

@JSExportTopLevel("LayoutsSVGFile", "cli")
object LayoutsSVGFile extends LayoutsScalatags(Text):

  @js.native
  trait TextMetrics extends js.Object:
    val actualBoundingBoxLeft: Double
    val actualBoundingBoxRight: Double
    val actualBoundingBoxAscent: Double
    val actualBoundingBoxDescent: Double
    val emHeightAscent: Double
    val emHeightDescent: Double

  @js.native
  trait CanvasRenderingContext2D extends js.Object:
    var font: String
    def measureText(text: String): TextMetrics = js.native

  @js.native
  trait Canvas extends js.Object:
    def getContext(`type`: String): CanvasRenderingContext2D = js.native

  @js.native
  @JSImport("canvas", "createCanvas")
  def createCanvas(width: Double, height: Double, `type`: String = "svg"): Canvas = js.native

  @js.native
  trait WritableStream extends js.Object:
    def write(chunk: String): Unit = js.native
    def `end`(): Unit = js.native

  @js.native
  @JSImport("fs", "createWriteStream")
  def createWriteStream(filename: String): WritableStream = js.native


  val textMetricsContext = createCanvas(1000, 100).getContext("2d")

  override protected def computeMetrics(text: String, font: FontInfo) = new Metrics:
    textMetricsContext.font = font.toCSSFont
    private val metrics = textMetricsContext.measureText(text)
    val width = metrics.actualBoundingBoxLeft + metrics.actualBoundingBoxRight
    val inkAscent = metrics.actualBoundingBoxAscent
    val inkDescent = metrics.actualBoundingBoxDescent
    val textAscent = metrics.emHeightAscent
    val textDescent = metrics.emHeightDescent
    lazy val capAscent = textMetricsContext.measureText(capProbe).actualBoundingBoxAscent
    lazy val exAscent = textMetricsContext.measureText(exProbe).actualBoundingBoxAscent

  def renderToFile(renderingInner: Text.Tag, stylesheet: String, filename: String) =
    val ws = createWriteStream(filename)
    ws.write(Text.svgTags.svg(
      renderingInner,
      Text.tags2.style(stylesheet),
      Text.attrs.xmlns:="http://www.w3.org/2000/svg").render)
    ws.`end`()
