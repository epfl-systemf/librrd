package librrd

import org.scalajs.dom.{SVGTextElement, document}
import scalatags.JsDom.all.*
import scalatags.JsDom.svgTags.*
import scalatags.JsDom.svgAttrs.width as svgWidth
import scalatags.JsDom.svgAttrs.height as svgHeight
import scalatags.JsDom.svgAttrs.{x, y, x1, x2, y1, y2, d, transform}

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

  val unitWidth = Layout.unitWidth
  val radius = 2*unitWidth

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

      case ivc @ InlineVerticalConcatenation(sublayouts, marker, tipSpecs, numRows, extraWidths, _, _) =>
        val (firsts, rests) = sublayouts.splitAt(1)
        val (mids, lasts) = rests.splitAt(rests.length - 1)
        val (first, last) = (firsts(0), lasts(0))
        val offsets = mids.scanLeft(first.height + Layout.rowGap)
          ((offset, sub) => offset + sub.height + Layout.rowGap)
        val padding = InlineVerticalConcatenation.markerPadding
        val direction = ivc.direction

        val inners =
          g(
            render(first),
            text(
              marker,
              x:=(direction match { case Direction.LTR => first.width; case Direction.RTL => 0 })
                + padding,
              y:=first.tipY(ivc.endSide)),
            transform:=s"translate(${extraWidths.left},0)"
          )
          +: mids.zip(offsets).map((mid, offset) =>
            g(
              text(marker, x:=padding, y:=mid.tipY(Side.Left)),
              g(render(mid), transform:=s"translate(${ivc.markerWidth},0)"),
              text(marker, x:=mid.width + 3*padding, y:=mid.tipY(Side.Right)),
              transform:=s"translate(${extraWidths.left},$offset)"
            ))
          :+ g(
            text(
              marker,
              x:=(direction match { case Direction.LTR => 0; case Direction.RTL => last.width })
                + padding,
              y:=last.tipY(ivc.startSide)),
            render(last),
            transform:=s"translate(${extraWidths.left},${offsets.last})"
          )

        val brackets = List((Side.Left, extraWidths.left, +1),
                            (Side.Right, extraWidths.left + first.width + ivc.markerWidth, -1))
          .flatMap((side, x, sign) =>
            val upwards: Int = -(sign - 1)/2
            val downwards = 1 - upwards
            val extraP = SidedProperty.apply.tupled(direction.swap((upwards, downwards)))
            val tipY = ivc.tipY(side)
            tipSpecs(side) match
              case TipSpecification.Physical(p) if p != extraP(side) =>
                def upPath(subTipY: Double) = path(d:=
                  s"M ${x - sign*3*unitWidth},$tipY  l ${sign*unitWidth},$tipY "
                  + s"a $radius,$radius 0 $upwards ${sign*2*unitWidth},${-2*unitWidth} "
                  + s"L $x,${subTipY + 2*unitWidth} "
                  + s"a $radius,$radius 0 $downwards ${sign*2*unitWidth},${-2*unitWidth}")
                def downPath(subTipY: Double) = path(d:=
                  s"M ${x - sign*3*unitWidth},$tipY  l ${sign*unitWidth},$tipY "
                  + s"a $radius,$radius 0 $downwards ${sign*2*unitWidth},${2*unitWidth} "
                  + s"L $x,${subTipY - 2*unitWidth} "
                  + s"a $radius,$radius 0 $upwards ${sign*2*unitWidth},${2*unitWidth}")
                val straightPath =
                  path(d:=s"M ${x - sign*3*unitWidth},$tipY  l ${sign*5*unitWidth},$tipY")

                val (ups, notUps) = (1 to numRows(side))
                  .map(r => ivc.tipY(side, TipSpecification.Logical(r)))
                  .partition(_ <= tipY - 4*unitWidth)
                val (notUpDowns, downs) = notUps.partition(_ <= tipY + 4*unitWidth)
                val (straights, approxs) = notUpDowns.partition(_ ~= tipY)
                straights.map(_ => straightPath) ++ ups.map(upPath) ++ downs.map(downPath)
              case _ => List())

        inners ++ brackets

      case bvc @ BlockVerticalConcatenation(
          topSublayout, bottomSublayout, direction, polarity, tipSpecs, numRows, _, _, _) =>
        import TipSpecification.*
        val leftExtraWidth = bvc.extraWidths.left

        val brackets = List((Side.Left, leftExtraWidth, +1),
                            (Side.Right, leftExtraWidth + topSublayout.width, -1))
          .flatMap((side, x, sign) =>
            val upwards: Int = -(sign - 1)/2
            val downwards = 1 - upwards
            val tipY = bvc.tipY(side)
            polarity match
              case Polarity.+ => tipSpecs(side) match
                case Vertical => List()
                case _ =>
                  def upPath(subTipY: Double) = path(d:=
                    s"M ${x - sign*3*unitWidth},$tipY  l ${sign*unitWidth},$tipY "
                    + s"a $radius,$radius 0 $upwards ${sign*2*unitWidth},${-2*unitWidth} "
                    + s"L $x,${subTipY + 2*unitWidth} "
                    + s"a $radius,$radius 0 $downwards ${sign*2*unitWidth},${-2*unitWidth}")
                  def downPath(subTipY: Double) = path(d:=
                    s"M ${x - sign*3*unitWidth},$tipY  l ${sign*unitWidth},$tipY "
                    + s"a $radius,$radius 0 $downwards ${sign*2*unitWidth},${2*unitWidth} "
                    + s"L $x,${subTipY - 2*unitWidth} "
                    + s"a $radius,$radius 0 $upwards ${sign*2*unitWidth},${2*unitWidth}")
                  val straightPath =
                    path(d:=s"M ${x - sign*3*unitWidth},$tipY  l ${sign*5*unitWidth},$tipY")

                  val (ups, notUps) = (1 to numRows(side))
                    .map(r => bvc.tipY(side, Logical(r)))
                    .partition(_ <= tipY - 4*unitWidth)
                  val (notUpDowns, downs) = notUps.partition(_ <= tipY + 4*unitWidth)
                  val (straights, approxs) = notUpDowns.partition(_ ~= tipY)
                  straights.map(_ => straightPath) ++ ups.map(upPath) ++ downs.map(downPath)

              case Polarity.- =>
                def topPath(subTipY: Double) = path(d:=
                  s"M $x,${bvc.bottomOffset}  L $x,${subTipY + 2*unitWidth} "
                  + s"a $radius,$radius 0 $downwards ${sign*2*unitWidth},${-2*unitWidth}")
                def bottomPath(subTipY: Double) = path(d:=
                  s"M $x,${bvc.bottomOffset}  L $x,${subTipY - 2*unitWidth} "
                  + s"a $radius,$radius 0 $upwards ${sign*2*unitWidth},${2*unitWidth}")
                val inners = (1 to topSublayout.numRows(side))
                  .map(r => topPath(bvc.tipY(side, Logical(r))))
                  ++ (1 to bottomSublayout.numRows(side))
                  .map(r => bottomPath(bvc.bottomOffset + bvc.tipY(side, Logical(r))))
                val outerPath = path(d:=
                  s"M ${x - sign*3*unitWidth},$tipY  l ${sign*unitWidth},$tipY "
                  + s"a $radius,$radius 0 $upwards ${sign*2*unitWidth},${-2*unitWidth}")
                val straight = path(d:=
                  s"M ${x - sign*3*unitWidth},$tipY  l ${sign*5*unitWidth},$tipY")

                tipSpecs(side) match
                  case Vertical => inners
                  case Physical(0) | Logical(1) =>
                    if topSublayout.numRows(side) == 1 then straight +: inners
                    else straight +: outerPath +: inners
                  case _ => outerPath +: inners)

        List(
          g(render(topSublayout), transform:=s"translate($leftExtraWidth,0)"),
          g(render(bottomSublayout), transform:=s"translate($leftExtraWidth,${bvc.bottomOffset})")
        ) ++ brackets

    val withGroup = inner
      :+ rect(x:=0, y:=0, svgWidth:=layout.width, svgHeight:=layout.height, `class`:="librrd-group")
      :+ (`class`:=(layout.classes).mkString(" "))
      :+ (id:=layout.id)
    g(withGroup*)
