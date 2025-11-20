package librrd

import scalajs.js
import js.annotation.*

@JSExportTopLevel("SBlockLayoutsSVGFile", "cli")
object SBlockLayoutsSVGFile extends SBlockLayoutsScalatags(scalatags.Text):

  @js.native
  trait TextMetrics extends js.Object:
    val actualBoundingBoxLeft: Double
    val actualBoundingBoxRight: Double
    val actualBoundingBoxAscent: Double
    val actualBoundingBoxDescent: Double

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


  override val baselineCorrection = -1
  val textMetricsContext = createCanvas(1000, 100).getContext("2d")
  override def measure(text: String, font: FontInfo) =
    textMetricsContext.font = font.toCSSFont
    val metrics = textMetricsContext.measureText(text)
    (metrics.actualBoundingBoxLeft + metrics.actualBoundingBoxRight,
     metrics.actualBoundingBoxAscent + metrics.actualBoundingBoxDescent)

  def renderToFile(renderingInner: String, stylesheet: String,
                   width: Double, height: Double, filename: String) =
    val rendering =
      """<svg xmlns="http://www.w3.org/2000/svg">"""
      + "<style>" + stylesheet + "</style>"
      + renderingInner
      + """</svg>"""
    val ws = createWriteStream(filename)
    ws.write(rendering)
    ws.`end`()
