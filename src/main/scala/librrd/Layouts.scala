package librrd

import org.scalajs.dom.{SVGTextElement, document}
import scalatags.JsDom.all.*
import scalatags.JsDom.svgTags.*
import scalatags.JsDom.svgAttrs
import scalatags.JsDom.svgAttrs.{cx, cy, r, x, y, radius, x1, x2, y1, y2, d, rx, ry, style}

trait LayoutInfo:
  val `class`: String

enum Direction { case LTR ; case RTL }

abstract class Layout(
    val dir: Direction,
    val classes: Seq[String] = Seq.empty,
    val ID: String = freshID()):
  final def render: Tag =
    renderInner(`class`:=(classes :+ Layout.`class`).mkString(" "), id:=ID)
  def renderInner: Tag

object Layout:
  val unitWidth = 4
  val `class` = "librrd"

  private var lastID = 0
  val generatedIDPrefix = "librrd-generated-"

  def freshID(): String =
    lastID += 1
    generatedIDPrefix + lastID.toString()

  def resetID(): Unit = lastID = 0

  lazy val textMetricsElement =
    document.createElementNS("http://www.w3.org/2000/svg", "text").asInstanceOf[SVGTextElement]
  textMetricsElement.style.setProperty("visibility", "hidden")
  textMetricsElement.style.setProperty("fill", "black")
  document.getElementById("output-canvas").appendChild(textMetricsElement)

  def measure(text: String) =
    textMetricsElement.textContent = text
    textMetricsElement.getBBox()


object Rail extends LayoutInfo:
  val `class` = "librrd-rail"

class Rail(val width: Double,
           direction: Direction,
           classes: Seq[String] = Seq.empty,
           ID: String = Layout.freshID())
    extends Layout(direction, classes :+ Rail.`class`, ID):
  override val renderInner =
    line(x1:=0, y1:=0, x2:=width, y2:=0)


object Space extends LayoutInfo:
  val `class` = "librrd-space"

class Space(val width: Double,
            direction: Direction,
            classes: Seq[String] = Seq.empty,
            ID: String = Layout.freshID())
    extends Layout(direction, classes :+ Space.`class`, ID):
  override val renderInner = path(d:="none")


object Station extends LayoutInfo:
  val `class` = "librrd-station"
  val terminalClass = "librrd-terminal"
  val nonterminalClass = "librrd-nonterminal"

  val paddingX = Layout.unitWidth
  val paddingY = Layout.unitWidth

class Station(val label: String,
              val isTerminal: Boolean,
              direction: Direction,
              classes: Seq[String] = Seq.empty,
              ID: String = Layout.freshID())
    extends Layout(
      direction,
      classes
        :+ Station.`class`
        :+ (if isTerminal then Station.terminalClass else Station.nonterminalClass),
      ID):
  val textBBox = Layout.measure(label)
  val width = textBBox.width + 4*Station.paddingX
  val height = textBBox.height + 2*Station.paddingY

  override val renderInner =
    g(
      rect(x:=Station.paddingX, y:=0, svgAttrs.width:=textBBox.width + 2*Station.paddingX, svgAttrs.height:=height),
      text("hello", x:=2*Station.paddingX, y:=Station.paddingY + textBBox.height),
      line(x1:=0, y1:=height/2, x2:=Station.paddingX, y2:=height/2, `class`:=Rail.`class`),
      line(x1:=width - Station.paddingX, y1:=height/2, x2:=width, y2:=height/2, `class`:=Rail.`class`),
    )
