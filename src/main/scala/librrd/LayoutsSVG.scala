package librrd

import org.scalajs.dom.{SVGTextElement, document}
import scalatags.JsDom.all.*
import scalatags.JsDom.svgTags.*
import scalatags.JsDom.svgAttrs.width as svgWidth
import scalatags.JsDom.svgAttrs.height as svgHeight
import scalatags.JsDom.svgAttrs.{cx, cy, r, x, y, radius, x1, x2, y1, y2, d, rx, ry, style, transform}

object LayoutsSVG extends Layouts[Tag]:

  lazy val textMetricsElement =
    val elem = document.createElementNS("http://www.w3.org/2000/svg", "text")
      .asInstanceOf[SVGTextElement]
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
      case rail: Rail => List(line(x1:=0, y1:=0, x2:=rail.width, y2:=0))
      case _: Space => List(path(d:="none"))
      case station: Station =>
        val width = station.width
        val height = station.height
        List(
          rect(x:=Station.paddingX, y:=0,
               svgWidth:=width - 2*Station.paddingX, svgHeight:=height),
          text(station.label, x:=2*Station.paddingX, y:=height - Station.paddingY),
          line(x1:=0, y1:=height/2, x2:=Station.paddingX, y2:=height/2,
               `class`:=Rail.`class`),
          line(x1:=width - Station.paddingX, y1:=height/2, x2:=width, y2:=height/2,
               `class`:=Rail.`class`),
        )
      case hc: HorizontalConcatenation =>
        hc.sublayouts.zip(hc.subXs.zip(hc.subYs)).map{ case (sub, (subX, subY)) =>
          g(render(sub), transform:=s"translate($subX,$subY)")
        }
      case ivc: InlineVerticalConcatenation =>
        ???
      case bvc @ BlockVerticalConcatenation(
          topSublayout, bottomSublayout, direction, polarity, tipSpecs, numRows, _, _, _) =>
        import TipSpecification.*
        val leftExtraWidth = (tipSpecs(Side.Left), polarity) match
          case (Vertical, Polarity.+) => 0
          case (Vertical | Logical(1) | Physical(0), Polarity.-) => 2 * Layout.unitWidth
          case _ => 3 * Layout.unitWidth

        val unitWidth = Layout.unitWidth
        val radius = 2*unitWidth
        val brackets = polarity match
          case Polarity.+ =>
            List((Side.Left, leftExtraWidth, +1),
                 (Side.Right, leftExtraWidth + topSublayout.width, -1))
              .map((side, x, sign) =>
                val topmostTipY = bvc.tipY(side, Physical(0))
                val bottommostTipY = bvc.tipY(side, Physical(1))
                val upwards: Int = -(sign - 1)/2
                val downwards = 1 - upwards
                tipSpecs(side) match
                  case Vertical => List()

                  case Physical(p) =>
                    val tipY = bvc.tipY(side)
                    val upPath = path(d:=
                      s"M ${x - sign*3*unitWidth},$tipY  l ${sign*unitWidth},$tipY "
                      + s"a $radius,$radius 0 $upwards ${sign*2*unitWidth},${-2*unitWidth} "
                      + s"L $x,${topmostTipY + 2*unitWidth} "
                      + s"a $radius,$radius 0 $downwards ${sign*2*unitWidth},${-2*unitWidth}")
                    val downPath = path(d:=
                      s"M ${x - sign*3*unitWidth},$tipY  l ${sign*unitWidth},$tipY "
                      + s"a $radius,$radius 0 $downwards ${sign*2*unitWidth},${2*unitWidth} "
                      + s"L $x,${bottommostTipY - 2*unitWidth} "
                      + s"a $radius,$radius 0 $downwards ${sign*2*unitWidth},${2*unitWidth}")
                    val straightPath =
                      path(d:=s"M ${x - sign*3*unitWidth},$tipY  l ${sign*5*unitWidth},$tipY")

                    if p == 0 then List(downPath, straightPath)
                    else if p == 1 then List(upPath, straightPath)
                    else List(upPath, downPath)

                  case Logical(r) => ???)
          case Polarity.- => ???

        

        List(
          g(render(topSublayout), transform:=s"translate($leftExtraWidth,0)"),
          g(render(bottomSublayout), transform:=s"translate($leftExtraWidth,${bvc.bottomOffset})")
        )

    val withGroup = inner
      :+ rect(x:=0, y:=0, svgWidth:=layout.width, svgHeight:=layout.height, `class`:="librrd-group")
      :+ (`class`:=(layout.classes).mkString(" "))
      :+ (id:=layout.id)
    g(withGroup*)
