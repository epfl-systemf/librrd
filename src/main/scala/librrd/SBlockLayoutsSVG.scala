package librrd

import scalatags.generic.{TypedTag, Bundle}

abstract class SBlockLayoutsScalatags[Builder, Output <: FragT, FragT]
    (bundle: Bundle[Builder, Output, FragT]) extends SBlockLayouts[TypedTag[Builder, Output, FragT]]:
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
      case lb: LineBreak =>
        val halfHeight = lb.startHeight + lb.middleHeight/2
        val p = path(d:=
            s"M ${lb.startOffset},${lb.tipY(RelativeSide.Start) + radius} "
          + s"L ${lb.startOffset},${halfHeight - radius} $quarterArc 1 ${-radius},$radius"
          + s"L $radius,${halfHeight} $quarterArc 0 ${-radius},$radius"
          + s"L 0,${lb.tipY(RelativeSide.End) - radius}")
        lb.direction match
          case Direction.RTL => List(g(p, transform:=s"translate(${lb.width},0) scale(-1,1)"))
          case _ => List(p)
      case hc: HorizontalConcatenation =>
        val (startSide, endSide) = hc.direction.swap((Side.Left, Side.Right))
        val connectorArgs = List(
          (startSide, 0,
            (_: Double, y: Double) => s"M 0,${y - radius}  $quarterArc 0 $radius,$radius"),
          (endSide, hc.sublayouts.length - 1,
            (w: Double, y: Double) => s"M ${w - radius},$y  $quarterArc 1 $radius,$radius"))
        hc.sublayouts.zipWithIndex.zip(hc.subXs.zip(hc.subYs)).map{
          case ((sub, i), (subX, subY)) =>
            val connectors = connectorArgs.flatMap{ case (side, sideI, sidePath) =>
              if sub.tipSpecs(side) == TipSpecification.Vertical && i != sideI then
                (1 to sub.tipRows(side))
                  .map(r => sub.tipY(side, TipSpecification.Logical(r)))
                  .map(y => path(d:=sidePath(sub.width, y)))
              else List() }
            val group =
              (hc.direction match
                case Direction.RTL =>
                  List(g(connectors, transform:=s"translate(${sub.width},0) scale(-1,1)"))
                case _ => connectors)
              :+ render(sub)
              :+ (transform:=s"translate($subX,$subY)")
            g(group*)
        }

/*
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
*/
    val withGroup = (inner
      :+ rect(x:=(-unitWidth), y:=(-2*unitWidth),
              svgWidth:=layout.width + 2*unitWidth, svgHeight:=layout.height + 4*unitWidth,
              `class`:="librrd-group")
      :+ (`class`:=(layout.classes).mkString(" ")))
      ++ layout.id.map(id:=_).toList
    g(withGroup*)


object SBlockLayoutsSVG extends SBlockLayoutsScalatags(scalatags.JsDom)
  with SVGTextMetrics
