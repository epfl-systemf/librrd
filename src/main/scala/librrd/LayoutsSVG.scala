package librrd

import org.scalajs.dom.{SVGTextElement, document}
import scalatags.JsDom.all.*
import scalatags.JsDom.svgTags.*
import scalatags.JsDom.svgAttrs
import scalatags.JsDom.svgAttrs.{cx, cy, r, x, y, radius, x1, x2, y1, y2, d, rx, ry, style, transform}

object LayoutsSVG extends Layouts[Tag]:

  lazy val textMetricsElement =
    val elem = document.createElementNS("http://www.w3.org/2000/svg", "text").asInstanceOf[SVGTextElement]
    elem.style.setProperty("visibility", "hidden")
    elem.style.setProperty("fill", "black")
    document.getElementById("output-canvas").appendChild(elem)
    elem

  override def measure(text: String) =
    textMetricsElement.textContent = text
    val bbox = textMetricsElement.getBBox()
    (bbox.width, bbox.height)

  override def render(layout: Layout) =
    val inner = layout match
      case rail: Rail => line(x1:=0, y1:=0, x2:=rail.width, y2:=0)
      case _: Space => path(d:="none")
      case station: Station =>
        val width = station.width
        val height = station.height
        g(
          rect(x:=Station.paddingX, y:=0, svgAttrs.width:=width - 2*Station.paddingX, svgAttrs.height:=height),
          text(station.label, x:=2*Station.paddingX, y:=height - Station.paddingY),
          line(x1:=0, y1:=height/2, x2:=Station.paddingX, y2:=height/2, `class`:=Rail.`class`),
          line(x1:=width - Station.paddingX, y1:=height/2, x2:=width, y2:=height/2, `class`:=Rail.`class`),
        )
      case hc: HorizontalConcatenation =>
        g(hc.sublayouts.zip(hc.subXs.zip(hc.subYs)).map{ case (sub, (subX, subY)) =>
          g(render(sub), transform:=s"translate($subX,$subY)")
        })
    g(
      inner,
      rect(x:=0, y:=0, svgAttrs.width:=layout.width, svgAttrs.height:=layout.height, `class`:="librrd-group"),
      `class`:=(layout.classes :+ Layout.`class`).mkString(" "), id:=layout.id,
    )
