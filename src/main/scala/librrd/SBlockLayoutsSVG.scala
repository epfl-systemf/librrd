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
      case lb: LineBreak =>
        val halfHeight = lb.startHeight + lb.middleHeight/2
        val (startSide, endSide) = lb.direction.swap((Side.Left, Side.Right))
        List(path(d:=
            s"M ${lb.startOffset},${lb.tipY(startSide) + radius}  "
          + s"L ${lb.startOffset},${halfHeight - radius}  $quarterArc 1 ${-radius},$radius"
          + s"L $radius,${halfHeight}  $quarterArc 0 ${-radius},$radius"
          + s"L 0,${lb.tipY(endSide) - radius}",
          transform:=(lb.direction match
            case Direction.RTL => s"translate(${lb.width},0) scale(-1,1)"
            case _ => ""),
          `class`:=Rail.`class`))

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
                  .map(y => path(d:=sidePath(sub.width, y), `class`:=Rail.`class`))
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

      case bhc: BlockedHorizontalConcatenation =>

        val extraP = SidedProperty(bhc.direction, 0, 1)
        val extraWidths = bhc.extraWidths
        g(render(bhc.hc), transform:=s"translate(${extraWidths.left},0)")
        +: List((Side.Left, extraWidths.left, +1),
                (Side.Right, bhc.width - extraWidths.right, -1))
          .flatMap((side, x, sign) =>
            bhc.tipSpecs(side) match
              case TipSpecification.Physical(p) if p != extraP(side) =>
                positiveBrackets(
                  bhc.tipY(side),
                  (1 to bhc.hc.sidemosts(side).tipRowsPossible(side))
                    .map(r => bhc.hc.tipY(side, TipSpecification.Logical(r))),
                  sign,
                  x)
              case _ => List())

    val withGroup = (inner
      :+ rect(x:=(-unitWidth), y:=(-2*unitWidth),
              svgWidth:=layout.width + 2*unitWidth, svgHeight:=layout.height + 4*unitWidth,
              `class`:="librrd-group")
      :+ (`class`:=(layout.classes).mkString(" ")))
      ++ layout.id.map(id:=_).toList
    g(withGroup*)


object SBlockLayoutsSVG extends SBlockLayoutsScalatags(scalatags.JsDom)
  with SVGTextMetrics
