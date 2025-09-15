package librrd

import scalatags.generic.{TypedTag, Bundle}

abstract class LayoutsScalatags[Builder, Output <: FragT, FragT]
    (bundle: Bundle[Builder, Output, FragT]) extends Layouts[TypedTag[Builder, Output, FragT]]:
  import bundle.all.*
  import bundle.svgTags.*
  import bundle.svgAttrs.width as svgWidth
  import bundle.svgAttrs.height as svgHeight
  import bundle.svgAttrs.{x, y, x1, x2, y1, y2, rx, ry, d, transform}

  def fontToStyleString(font: FontInfo) =
    s"font-family: ${font.family}; " +
    s"font-size: ${font.size}; " +
    s"font-weight: ${font.weight}; " +
    s"font-style: ${font.style};"

  val baselineCorrection = -2.0
  val unitWidth = Layout.unitWidth
  val radius = 2*unitWidth
  val quarterArc = s"a $radius,$radius 0 0"

  def positiveBrackets(tipY: Double, subTipYs: Seq[Double], sign: Int, x: Double) =
    val upwards: Int = -(sign - 1)/2
    val downwards = 1 - upwards

    def upPath(subTipY: Double) = path(d:=
      s"M ${x - sign*3*unitWidth},$tipY  l ${sign*unitWidth},0 "
      + s"$quarterArc $upwards ${sign*radius},${-radius} "
      + s"L $x,${subTipY + 2*unitWidth} "
      + s"$quarterArc $downwards ${sign*radius},${-radius}")
    def downPath(subTipY: Double) = path(d:=
      s"M ${x - sign*3*unitWidth},$tipY  l ${sign*unitWidth},0 "
      + s"$quarterArc $downwards ${sign*radius},$radius "
      + s"L $x,${subTipY - 2*unitWidth} "
      + s"$quarterArc $upwards ${sign*radius},$radius")
    val straightPath =
      path(d:=s"M ${x - sign*3*unitWidth},$tipY  l ${sign*5*unitWidth},0")
    def approxPath(subTipY: Double) =
      val halfTip = (subTipY - tipY)/2
      val theta = 2*Math.atan(halfTip/radius)
      val r = radius/Math.sin(theta)
      val (first, second) = if halfTip < 0 then (upwards, downwards) else (downwards, upwards)
      path(d:=s"M ${x - sign*3*unitWidth},$tipY  l ${sign*unitWidth},0 "
            + s"a $r,$r 0 0 $first ${sign*radius},$halfTip "
            + s"a $r,$r 0 0 $second ${sign*radius},$halfTip")

    val (ups, notUps) = subTipYs.partition(_ <= tipY - 2*radius)
    val (notUpDowns, downs) = notUps.partition(_ <= tipY + 2*radius)
    val (straights, approxs) = notUpDowns.partition(_ ~= tipY)
    straights.map(_ => straightPath) ++ ups.map(upPath) ++ downs.map(downPath)
      ++ approxs.map(approxPath)

  override def render(layout: Layout) =
    val inner = layout match
      case rail: Rail => List(line(x1:=0, y1:=0, x2:=rail.width, y2:=0))
      case _: Space => List(path(d:=""))
      case station: Station =>
        val width = station.width
        val height = station.height
        val rounded = if station.isTerminal then radius else 0
        List(
          rect(x:=Station.paddingX, y:=0, rx:=rounded, ry:=rounded,
               svgWidth:=width - 2*Station.paddingX, svgHeight:=height),
          text(station.label, x:=2*Station.paddingX,
               y:=height - Station.paddingY + baselineCorrection,
               style:=fontToStyleString(station.font)),
          line(x1:=0, y1:=height/2, x2:=Station.paddingX, y2:=height/2,
               `class`:=Rail.`class`),
          line(x1:=width - Station.paddingX, y1:=height/2, x2:=width, y2:=height/2,
               `class`:=Rail.`class`),
        )
      case hc: HorizontalConcatenation =>
        hc.sublayouts.zip(hc.subXs.zip(hc.subYs)).map{ case (sub, (subX, subY)) =>
          g(render(sub), transform:=s"translate($subX,$subY)")
        }

      case ivc @ InlineVerticalConcatenation(sublayouts, marker, tipSpecs, numRows, extraWidths,
                                             font, _, _) =>
        val (first, mids, last) = splitEnds(sublayouts)
        val ends = SidedProperty(first, last)
        val offsets = mids.scanLeft(first.height + Layout.rowGap)
          ((offset, sub) => offset + sub.height + Layout.rowGap)
        val padding = InlineVerticalConcatenation.markerPadding
        val direction = ivc.direction
        val (firstMarkerX, firstX, lastMarkerX, lastX) = direction match
          case Direction.LTR => (first.width, 0.0, 0.0, ivc.markerWidth)
          case Direction.RTL => (0.0, ivc.markerWidth, last.width, 0.0)
        val markerElement = text(marker, style:=fontToStyleString(ivc.font))

        val inners =
          g(
            g(render(first), transform:=s"translate($firstX,0)"),
            markerElement(x:=firstMarkerX + padding, y:=first.tipY(ivc.endSide)),
            transform:=s"translate(${extraWidths.left},0)"
          )
          +: mids.zip(offsets).map((mid, offset) =>
            g(
              markerElement(x:=padding, y:=mid.tipY(Side.Left)),
              g(render(mid), transform:=s"translate(${ivc.markerWidth},0)"),
              markerElement(x:=ivc.markerWidth + mid.width + padding, y:=mid.tipY(Side.Right)),
              transform:=s"translate(${extraWidths.left},$offset)"
            ))
          :+ g(
            markerElement(x:=lastMarkerX + padding, y:=last.tipY(ivc.startSide)),
            g(render(last), transform:=s"translate($lastX,0)"),
            transform:=s"translate(${extraWidths.left},${offsets.last})"
          )

        val extraP = SidedProperty.apply.tupled(direction.swap((0, 1)))
        val brackets = List((Side.Left, extraWidths.left, +1),
                            (Side.Right, extraWidths.left + first.width + ivc.markerWidth, -1))
          .flatMap((side, x, sign) =>
            tipSpecs(side) match
              case TipSpecification.Physical(p) if p != extraP(side) =>
                positiveBrackets(
                  ivc.tipY(side),
                  (1 to ends(side).numRows(side))
                    .map(r => ivc.tipY(side, TipSpecification.Logical(r))),
                  sign,
                  x)
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
                case _ => positiveBrackets(
                            tipY,
                            (1 to (topSublayout.numRows(side) + bottomSublayout.numRows(side)))
                              .map(r => bvc.tipY(side, Logical(r))),
                            sign,
                            x)

              case Polarity.- =>
                def topPath(subTipY: Double) = path(d:=
                  s"M $x,${bvc.bottomOffset - Layout.rowGap/2}  L $x,${subTipY + 2*unitWidth} "
                  + s"$quarterArc $downwards ${sign*radius},${-radius}")
                def bottomPath(subTipY: Double) = path(d:=
                  s"M $x,${bvc.bottomOffset - Layout.rowGap/2}  L $x,${subTipY - 2*unitWidth} "
                  + s"$quarterArc $upwards ${sign*radius},$radius")
                val inners = (1 to topSublayout.numRows(side))
                  .map(r => topPath(topSublayout.tipY(side, Logical(r))))
                  ++ (1 to bottomSublayout.numRows(side))
                  .map(r => bottomPath(bvc.bottomOffset + bottomSublayout.tipY(side, Logical(r))))
                val outerPath = path(d:=
                  s"M ${x - sign*3*unitWidth},$tipY  l ${sign*unitWidth},0 "
                  + s"$quarterArc $upwards ${sign*radius},${-radius}")
                val straight = path(d:=
                  s"M ${x - sign*3*unitWidth},$tipY  l ${sign*5*unitWidth},0")

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
      :+ rect(x:=(-unitWidth), y:=(-2*unitWidth),
              svgWidth:=layout.width + 2*unitWidth, svgHeight:=layout.height + 4*unitWidth,
              `class`:="librrd-group")
      :+ (`class`:=(layout.classes).mkString(" "))
      :+ (id:=layout.id)
    g(withGroup*)


object LayoutsSVG extends LayoutsScalatags(scalatags.JsDom):
  import org.scalajs.dom.{SVGTextElement, document}

  lazy val textMetricsElement =
    val elem = document.createElementNS("http://www.w3.org/2000/svg", "text")
      .asInstanceOf[SVGTextElement]
    elem.style.setProperty("visibility", "hidden")
    elem.style.setProperty("fill", "black")
    document.getElementById("output-canvas").appendChild(elem)
    elem

  override def measure(text: String, font: FontInfo) =
    textMetricsElement.textContent = text
    textMetricsElement.style.setProperty("font-family", font.family)
    textMetricsElement.style.setProperty("font-size", font.size)
    textMetricsElement.style.setProperty("font-weight", font.weight)
    textMetricsElement.style.setProperty("font-style", font.style)
    val bbox = textMetricsElement.getBBox()
    (bbox.width, bbox.height)
