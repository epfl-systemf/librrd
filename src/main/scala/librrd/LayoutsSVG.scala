package librrd

import scalatags.generic.{TypedTag, Bundle}
import TipSpecification.*

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

  val unitWidth = Layout.unitWidth
  val radius = 2*unitWidth
  val quarterArc = s"a $radius,$radius 0 0"

  def rail(attrs: Modifier*) = path(attrs, `class`:=Rail.`class`)

  // Measurement results.  Using an abstract class allows us to implement fields
  // with lazy vals to avoid computing every value unnecessarily.
  abstract class Metrics:
    def width: Double
    def inkAscent: Double
    def inkDescent: Double
    def textAscent: Double
    def textDescent: Double
    def capAscent: Double
    def exAscent: Double

  protected def computeMetrics(text: String, font: FontInfo): Metrics

  val capProbe = "H"
  val exProbe = "x"

  override def measure(text: String, font: FontInfo,
                       edges: TextBoxEdges, trim: TextBoxTrimPolicy) =
    val m = computeMetrics(text, font)
    def overEdge(mode: TextBoxOverEdge): Double = mode match
      case TextBoxOverEdge.Text => m.textAscent
      case TextBoxOverEdge.Cap  => m.capAscent
      case TextBoxOverEdge.Ex   => m.exAscent
      case TextBoxOverEdge.Ink  => m.inkAscent
    def underEdge(mode: TextBoxUnderEdge): Double = mode match
      case TextBoxUnderEdge.Text       => m.textDescent
      case TextBoxUnderEdge.Alphabetic => 0.0
      case TextBoxUnderEdge.Ink        => m.inkDescent
    val asc  = if trim.trimStart then overEdge(edges.over)  else m.textAscent
    val desc = if trim.trimEnd   then underEdge(edges.under) else m.textDescent
    TextDimensions(m.width, asc, desc, m.inkAscent)


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
               y:=station.baselineY,
               style:=fontToStyleString(station.font)),
          line(x1:=0, y1:=height/2, x2:=Station.paddingX, y2:=height/2,
               `class`:=Rail.`class`),
          line(x1:=width - Station.paddingX, y1:=height/2, x2:=width, y2:=height/2,
               `class`:=Rail.`class`),
        )
      case lb: LineBreak =>
        val halfHeight = lb.startHeight + lb.middleHeight/2
        val (startSide, endSide) = lb.direction.swap((Side.Left, Side.Right))

        val rendering =
          if lb.marker.isDefined then
            val markerPadding = LineBreak.markerPadding
            val markerElement = text(lb.marker, style:=fontToStyleString(lb.font))
            g(markerElement(x:=lb.startOffset, y:=lb.tipY(startSide) + 5),
              markerElement(x:=markerPadding, y:=lb.tipY(endSide) + 5))
          else path(`class`:=Rail.`class`, d:=
              s"M ${lb.startOffset},${lb.tipY(startSide) + radius}  "
            + s"L ${lb.startOffset},${halfHeight - radius}  $quarterArc 1 ${-radius},$radius  "
            + s"L ${lb.endWidth + radius},${halfHeight}  $quarterArc 0 ${-radius},$radius  "
            + s"L ${lb.endWidth},${lb.tipY(endSide) - radius}")

        List(rendering(
          transform:=(lb.direction match
            case Direction.RTL => s"translate(${lb.width},0) scale(-1,1)"
            case _ => "")))

      case hc: HorizontalConcatenation =>
        val (startSide, endSide) = hc.direction.swap((Side.Left, Side.Right))
        hc.sublayouts.zipWithIndex.zip(hc.subXs.zip(hc.subYs)).map{
          case ((sub, i), (subX, subY)) =>
            val startConnector =
              if sub.tipSpecs(startSide) == Vertical && i != 0 && hc.connectors then
                val firstSubTip = sub.tipY(startSide, Logical(1))
                (1 to sub.tipRows(startSide))
                  .map(r => sub.tipY(startSide, Logical(r)))
                  .map(y => rail(d:=s"M 0,${firstSubTip - radius}  L 0,${y - radius}  "
                    + s"$quarterArc 0 $radius,$radius"))
              else List()
            val endConnector =
              if sub.tipSpecs(endSide) == Vertical
                  && i != hc.sublayouts.length - 1 && hc.connectors then
                val lastSubTip = sub.tipY(endSide, Logical(sub.tipRows(endSide)))
                (1 to sub.tipRows(endSide))
                  .map(r => sub.tipY(endSide, Logical(r)))
                  .map(y => rail(d:=s"M ${sub.width - radius},$y  $quarterArc 1 $radius,$radius  "
                    + s"L ${sub.width},${lastSubTip + radius}"))
              else List()

            val group =
              (hc.direction match
                case Direction.RTL => List(g(startConnector ++ endConnector,
                  transform:=s"translate(${sub.width},0) scale(-1,1)"))
                case _ => startConnector ++ endConnector)
              :+ render(sub)
              :+ (transform:=s"translate($subX,$subY)")
            g(group*)
        }

      case bhc @ BlockedHorizontalConcatenation(hc, _) =>
        val extraP = SidedProperty(bhc.direction, 0, 1)
        val extraWidths = bhc.extraWidths
        g(render(hc), transform:=s"translate(${extraWidths.left},0)")
        +: (if hc.sBlockRows == 1 then List()
            else List((Side.Left, extraWidths.left, +1),
                      (Side.Right, bhc.width - extraWidths.right, -1))
          .flatMap((side, x, sign) =>
            bhc.tipSpecs(side) match
              case Physical(p) if p != extraP(side) =>
                positiveBrackets(
                  bhc.tipY(side),
                  (1 to hc.sidemosts(side).tipRowsPossible(side))
                    .map(r => hc.tipY(side, Logical(r))),
                  sign,
                  x)
              case Physical(_) | Logical(_) =>
                List(rail(d:=s"M $x,${hc.tipY(side)}  l ${-sign*2*unitWidth},0"))
              case _ => List()))

      case vc @ VerticalConcatenation(
          topSublayout, bottomSublayout, direction, polarity, tipSpecs, rowGap, _, _) =>
        val leftExtraWidth = vc.extraWidths.left

        val brackets = List((Side.Left, leftExtraWidth, +1),
                            (Side.Right, leftExtraWidth + topSublayout.width, -1))
          .flatMap((side, x, sign) =>
            val upwards: Int = -(sign - 1)/2
            val downwards = 1 - upwards
            val tipY = vc.tipY(side)
            polarity match
              case Polarity.+ => tipSpecs(side) match
                case Vertical => List()
                case _ => positiveBrackets(tipY,
                  (1 to vc.tipRowsPossible(side)).map(r => vc.tipY(side, Logical(r))), sign, x)

              case _ =>
                def topPath(subTipY: Double) = path(d:=
                  s"M $x,${vc.bottomOffset - rowGap/2}  L $x,${subTipY + 2*unitWidth} "
                  + s"$quarterArc $downwards ${sign*radius},${-radius}")
                def bottomPath(subTipY: Double) = path(d:=
                  s"M $x,${vc.bottomOffset - rowGap/2}  L $x,${subTipY - 2*unitWidth} "
                  + s"$quarterArc $upwards ${sign*radius},$radius")
                val inners = (1 to topSublayout.tipRowsPossible(side))
                  .map(r => topPath(topSublayout.tipY(side, Logical(r))))
                  ++ (1 to bottomSublayout.tipRowsPossible(side))
                  .map(r => bottomPath(vc.bottomOffset + bottomSublayout.tipY(side, Logical(r))))
                val outerPath = rail(d:=
                  s"M ${x - sign*3*unitWidth},$tipY  l ${sign*unitWidth},0 "
                  + (polarity match
                       case Polarity.-? => s"$quarterArc $upwards ${sign*radius},${-radius}"
                       case Polarity.-! => s"$quarterArc $downwards ${sign*radius},$radius"
                       case _ => ??? /* unreachable */))
                val straight = rail(d:=
                  s"M ${x - sign*3*unitWidth},$tipY  l ${sign*5*unitWidth},0")

                tipSpecs(side) match
                  case Vertical => inners
                  case Physical(0) | Logical(1) if polarity == Polarity.-? =>
                    if topSublayout.tipRowsPossible(side) == 1
                    then straight +: inners
                    else straight +: outerPath +: inners
                  case Physical(1) if polarity == Polarity.-! =>
                    if bottomSublayout.tipRowsPossible(side) == 1
                    then straight +: inners
                    else straight +: outerPath +: inners
                  case Logical(r) if polarity == Polarity.-! && r == vc.tipRowsPossible(side) =>
                    if bottomSublayout.tipRowsPossible(side) == 1
                    then straight +: inners
                    else straight +: outerPath +: inners
                  case _ => outerPath +: inners)

        g(render(topSublayout), transform:=s"translate($leftExtraWidth,0)")
        +: g(render(bottomSublayout), transform:=s"translate($leftExtraWidth,${vc.bottomOffset})")
        +: brackets


      case ve @ VerticalEpsilon(sub, width, direction, polarity, tipSpecs, _, _, _, _, _) =>
        val (startSide, endSide) = (ve.startSide, ve.endSide)
        val tipY = ve.tipY(startSide)
        val startX = ve.startOffset + ve.extraWidth
        val halfHeight = ve.startHeight + ve.middleHeight/2
        val startBrackets =
          (tipSpecs(startSide), polarity) match
             case (Vertical, _) =>
               (1 to sub.tipRows(startSide))
                 .map(r => sub.tipY(startSide, Logical(r)))
                 .map(y => rail(d:=s"M $startX,0  L $startX,${y - radius}  "
                   + s"$quarterArc 0 $radius,$radius"))
               :+ (polarity match
                 case Polarity.+ => rail(d:=s"M $startX,0  L $startX,${ve.startHeight}")
                 case _ => rail(d:=s"M ${startX + radius},${sub.tipY(startSide, Logical(1))} "
                   + s"$quarterArc 0 ${-radius},$radius  L $startX,${ve.startHeight}"))
             case (_, Polarity.+) =>
               positiveBrackets(tipY,
                 (1 to sub.tipRows(startSide)).map(r => sub.tipY(startSide, Logical(r))),
                 1, startX)
               :+ rail(d:=s"M ${ve.startOffset},$tipY  L ${startX - radius},$tipY  "
                 + s"$quarterArc 1 $radius,$radius  L $startX,${ve.startHeight}")
             case (_, _) =>
               val subPossible = sub.tipRows(startSide)
               (if subPossible > 1 then
                  positiveBrackets(tipY, // tipY must be lowest Logical
                    (1 to subPossible).map(r => sub.tipY(startSide, Logical(r))), 1, startX)
                  :+ rail(d:=s"M $startX,${ve.startHeight}  L $startX,${tipY + radius}  "
                    + s"$quarterArc 1 $radius,${-radius}")
                  :+ rail(d:=s"M $startX,${tipY + radius}  L $startX,${tipY - radius}")
                else List(
                  rail(d:=s"M ${ve.startOffset},$tipY l ${ve.extraWidth + radius},0"),
                  rail(d:=s"M $startX,${ve.startHeight}  L $startX,${tipY + radius}  "
                    + s"$quarterArc 1 $radius,${-radius}")))

        val endBrackets = (1 to sub.tipRows(endSide))
          .map(r => sub.tipY(endSide, Logical(r)))
          .map(y => rail(d:=s"M ${width - radius},$y  $quarterArc 1 $radius,$radius  "
            + s"L $width,${ve.startHeight}"))

        val linebreak =
          rail(d:=s"M $width,${ve.startHeight}  "
            + s"L $width,${halfHeight - radius}  $quarterArc 1 ${-radius},$radius  "
            + s"L ${ve.endWidth+radius},${halfHeight}  $quarterArc 0 ${-radius},$radius  "
            + s"L ${ve.endWidth},${ve.height - radius}")

        val linebreakConnector =
          if polarity != Polarity.+ then
            rail(d:=s"M $startX,${ve.startHeight}  L $startX,${halfHeight - radius}  "
            + s"$quarterArc 0 $radius,$radius")
          else if width - ve.extraWidth ~= sub.width then
            rail(d:=s"M $startX,${ve.startHeight}  L $startX,${halfHeight + radius}")
          else rail(d:=s"M $startX,${ve.startHeight}  L $startX,${halfHeight - radius}  "
            + s"$quarterArc 1 ${-radius},$radius")

        val rails = (startBrackets :+ linebreakConnector) ++ (sub match
          case _: BlockLayout => linebreak +: endBrackets
          case _ => List())
        ve.direction match
          case Direction.RTL => List(g(render(sub)),
            g(rails, transform:=s"translate($width,0) scale(-1,1)"))
          case _ => g(render(sub), transform:=s"translate($startX,0)") +: rails

      case lbl @ LabeledBlockLayout(sub, label, font, position) =>
        import LabelPositionInline.*
        import LabelPositionBlock.*
        val padY = LabeledBlockLayout.paddingY

        val labelMeasure = measure(label, font)
        val labelWidth = labelMeasure.width
        val dx = SidedProperty.forEach(s => sub.tipSpecs(s) match
          case Vertical => Layout.unitWidth
          case _ => 0)
        val labelX = (position.`inline`, lbl.direction) match
          case (Left, _) | (Start, Direction.LTR) | (End, Direction.RTL) => dx(Side.Left)
          case (Right, _) | (Start, Direction.RTL) | (End, Direction.LTR) => lbl.width - dx(Side.Right) - labelWidth
          case (Center, _) => (lbl.width + dx(Side.Left) - dx(Side.Right) - labelWidth)/2
        val (translateY, labelY) = position.block match
          case Top => (2*padY/3, padY/3)
          case Bottom => (padY/3, lbl.startHeight - padY/3 + labelMeasure.ascent)

        List(rect(x:=(-dx(Side.Left)), y:=(translateY - padY/6),
               svgWidth:=(sub.width + dx(Side.Left) + dx(Side.Right)), svgHeight:=sub.height + padY/3,
               `class`:="librrd-group"),
             g(render(sub), transform:=s"translate(0,$translateY)"),
             text(label, x:=labelX, y:=labelY, style:=fontToStyleString(font)))


    val withGroup = (inner
      :+ (`class`:=(layout.classes).mkString(" ")))
      ++ layout.id.map(id:=_).toList
    g(withGroup*)


object LayoutsSVG extends LayoutsScalatags(scalatags.JsDom):
  import org.scalajs.dom
  import scala.scalajs.js

  // The `TextMetrics` included in our version of scalajs-dom only has .width, so
  // we redefine `TextMetrics` here.
  @js.native
  trait TextMetrics extends js.Object:
    val actualBoundingBoxAscent: Double = js.native
    val actualBoundingBoxDescent: Double = js.native
    val width: Double = js.native

  // Firefox doesn't have `TextMetrics.emHeight`, so we approximate
  // ascender/descender by rendering a tall character and a deep character in a
  // hidden `<canvas>`.
  lazy val textMetricsContext =
    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
    dom.document.body.append(canvas)
    val bodyStyle = dom.window.getComputedStyle(dom.document.body)
    canvas.style.fontFamily = bodyStyle.fontFamily
    canvas.style.fontSize = bodyStyle.fontSize
    canvas.style.display = "none"
    canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  val tallProbe = "Áy"

  private def metricsOf(s: String): TextMetrics =
    textMetricsContext.measureText(s).asInstanceOf[TextMetrics]

  override protected def computeMetrics(text: String, font: FontInfo) = new Metrics:
    textMetricsContext.font = font.toCSSFont
    private val metrics = metricsOf(text)
    private lazy val tall = metricsOf(tallProbe)
    val width = metrics.width
    val inkAscent = metrics.actualBoundingBoxAscent
    val inkDescent = metrics.actualBoundingBoxDescent
    lazy val textAscent = tall.actualBoundingBoxAscent
    lazy val textDescent = tall.actualBoundingBoxDescent
    lazy val capAscent = metricsOf(capProbe).actualBoundingBoxAscent
    lazy val exAscent = metricsOf(exProbe).actualBoundingBoxAscent

  def renderToSVG(rendering: scalatags.JsDom.Tag): dom.SVGSVGElement =
    import scalatags.JsDom.svgTags.svg
    import scalatags.JsDom.attrs.xmlns
    import scalatags.JsDom.implicits.stringAttr
    svg(rendering, xmlns:="http://www.w3.org/2000/svg")
      .render.asInstanceOf[dom.SVGSVGElement]
