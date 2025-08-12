package librrd

import Side.*
import TipSpecification.*

def linearInterpolate(xStart: Double, yStart: Double,
                      xEnd: Double, yEnd: Double, x: Double): Double =
  yStart + (yEnd - yStart) * (x - xStart)/(xEnd - xStart)

trait Layouts[T]:

  def measure(text: String): (Double, Double)
  def render(layout: Layout): T

  private var lastID = 0
  val generatedIDPrefix = "librrd-generated-"

  def freshID(): String =
    lastID += 1
    generatedIDPrefix + lastID.toString()

  def resetID(): Unit = lastID = 0


  sealed trait Layout:
    val direction: Direction
    val id: String
    def width: Double
    def height: Double
    def classes: Set[String]
    val numRows: NumRows
    val tipSpecs: TipSpecifications

    final def tipY(s: Side, ts: TipSpecification): Double =
      ts match
        case Vertical => ()
        case Physical(p) =>
          assert(0 <= p && p <= 1,
            "if tip specification is physical, proportion must be between 0 and 1")
        case Logical(r) => ()
      tipYInner(s, ts)

    def tipYInner(s: Side, ts: TipSpecification): Double
    lazy val tipY: SidedProperty[Double] = SidedProperty.forEach(s => tipY(s, tipSpecs(s)))

  object Layout:
    val unitWidth = 4.0
    val `class` = "librrd"
    val rowGap = 2*unitWidth


  trait InlineLayout:
    val numRows = SidedProperty(1, 1)
    val tipSpecs = TipSpecifications(Logical(1), Logical(1))


  object Rail:
    val `class` = "librrd-rail"

  case class Rail(
      width: Double,
      direction: Direction,
      initClasses: Set[String] = Set.empty,
      initId: Option[String] = None) extends Layout with InlineLayout:
    val classes = initClasses + Rail.`class`
    val id = initId.getOrElse(freshID())
    val height = 0
    def tipYInner(s: Side, ts: TipSpecification) = 0


  object Space:
    val `class` = "librrd-space"

  case class Space(
      width: Double,
      direction: Direction,
      initClasses: Set[String] = Set.empty,
      initId: Option[String] = None) extends Layout with InlineLayout:
    val classes = initClasses + Space.`class`
    val id = initId.getOrElse(freshID())
    val height = 0
    def tipYInner(s: Side, ts: TipSpecification) = 0


  object Station:
    val `class` = "librrd-station"
    val terminalClass = "librrd-terminal"
    val nonterminalClass = "librrd-nonterminal"

    val paddingX = Layout.unitWidth
    val paddingY = Layout.unitWidth

  case class Station(
      label: String,
      isTerminal: Boolean,
      direction: Direction,
      initClasses: Set[String] = Set.empty,
      initId: Option[String] = None) extends Layout with InlineLayout:
    val (textWidth, textHeight) = measure(label)
    val width = textWidth + 4*Station.paddingX
    val height = textHeight + 2*Station.paddingY

    val classes = initClasses
      + Station.`class`
      + (if isTerminal then Station.terminalClass else Station.nonterminalClass)
    val id = initId.getOrElse(freshID())

    def tipYInner(s: Side, ts: TipSpecification) = height/2


  object HorizontalConcatenation:
    val `class` = "librrd-hconcat"

  case class HorizontalConcatenation(
      sublayouts: Seq[Layout],
      initClasses: Set[String] = Set.empty,
      initId: Option[String] = None) extends Layout with InlineLayout:

    val classes = initClasses + HorizontalConcatenation.`class`
    val id = initId.getOrElse(freshID())

    assert(!sublayouts.isEmpty, "horizontal concatenation must have at least 1 sublayout")
    assert(
      sublayouts.forall(_.direction == sublayouts(0).direction),
      "sublayouts of horizontal concatenation must all have same direction")
    val direction = sublayouts(0).direction

    val (rightTipY, height, subXs, width) =
      sublayouts.foldLeft((0.0, 0.0, List(0.0))){ (acc, sub) => acc match
        case (prevRightTipY, prevHeight, prevWidths) =>
          (sub.tipY(Right) + Math.max(0, prevRightTipY - sub.tipY(Left)),
           prevHeight
             + Math.max(0, sub.tipY(Left) - prevRightTipY)
             + Math.max(0, (sub.height - sub.tipY(Left)) - (prevHeight - prevRightTipY)),
           (sub.width + prevWidths.head) :: prevWidths)
      } match
        case (rty, h, widths) => (rty, h, widths.tail.reverse, widths.head)

    val (leftTipY, subYs) =
      sublayouts.foldRight((rightTipY, List.empty[Double])){ (sub, acc) => acc match
        case (prevLeftTipY, prevYs) =>
          val subY = prevLeftTipY - sub.tipY(Right)
          (subY + sub.tipY(Left), subY :: prevYs)
      }

    def tipYInner(s: Side, ts: TipSpecification) =
      s match
        case Left => subYs.head + sublayouts.head.tipY(s, ts)
        case Right => subYs.last + sublayouts.last.tipY(s, ts)

    override lazy val tipY = SidedProperty(leftTipY, rightTipY)


  object InlineVerticalConcatenation:
    val `class` = "librrd-vconcat-inline"
    val markerPadding = Layout.unitWidth

  case class InlineVerticalConcatenation(
      sublayouts: Seq[Layout],
      marker: String,
      tipSpecs: TipSpecifications,
      numRows: NumRows,
      extraWidths: SidedProperty[Double],
      initClasses: Set[String] = Set.empty,
      initId: Option[String] = None) extends Layout:

    val classes = initClasses + InlineVerticalConcatenation.`class`
    val id = initId.getOrElse(freshID())

    assert(sublayouts.length >= 2,
      "inline vertical concatenation must have at least 2 sublayouts")
    assert(
      sublayouts.forall(_.direction == sublayouts(0).direction),
      "sublayouts of inline vertical concatenation must all have same direction")
    val direction = sublayouts(0).direction
    val (startSide, endSide) = direction.swap(Left, Right)

    val markerWidth = measure(marker)._1 + 2*InlineVerticalConcatenation.markerPadding
    val width = sublayouts.head.width + markerWidth + extraWidths.left + extraWidths.right
    assert(sublayouts.head.width ~= sublayouts.last.width,
      "first and last sublayout of inline vertical concatenation must have same width")
    assert(sublayouts.drop(1).dropRight(1).forall(_.width + 2*markerWidth ~= width),
      "middle sublayouts of inline vertical concatenation must all have width "
      + "equal to the first minus the marker width")

    val height = sublayouts.map(_.height).sum + Layout.rowGap * (sublayouts.length - 1)

    override def tipYInner(s: Side, ts: TipSpecification): Double =
      ts match
        case Vertical => Double.NaN
        case Physical(p) => linearInterpolate(
          0, sublayouts.head.tipY(startSide, Physical(0)),
          1, height - sublayouts.last.height + sublayouts.last.tipY(endSide, Physical(1)), p)
        case Logical(r) =>
          if s == startSide then
            sublayouts.head.tipY(s, ts)
          else
            height - sublayouts.last.height + sublayouts.last.tipY(s, ts)


  object BlockVerticalConcatenation:
    val `class` = "librrd-vconcat-block"

  case class BlockVerticalConcatenation(
      topSublayout: Layout,
      bottomSublayout: Layout,
      direction: Direction,
      polarity: Polarity,
      tipSpecs: TipSpecifications,
      numRows: NumRows,
      extraWidths: SidedProperty[Double],
      initClasses: Set[String] = Set.empty,
      initId: Option[String] = None) extends Layout:

    val classes = initClasses + BlockVerticalConcatenation.`class`
    val id = initId.getOrElse(freshID())

    assert(topSublayout.width ~= bottomSublayout.width,
      "top and bottom sublayouts of block vertical concatenation must have same width " +
      s"${topSublayout.width} ${bottomSublayout.width}")
    val width = topSublayout.width + extraWidths.left + extraWidths.right

    assert(topSublayout.direction == (polarity match
        case Polarity.+ =>  bottomSublayout.direction
        case Polarity.- =>  bottomSublayout.direction.reverse),
      "bottom sublayout of block vertical concatenation must have direction equal to " +
      "top sublayout iff polarity is positive")

    val bottomOffset = topSublayout.height + Layout.rowGap
    val height = bottomOffset + bottomSublayout.height

    def tipYInner(s: Side, ts: TipSpecification): Double =
      ts match
        case Vertical => Double.NaN
        case Physical(p) => linearInterpolate(
          0, topSublayout.tipY(s, Physical(0)),
          1, bottomOffset + bottomSublayout.tipY(s, Physical(1)), p)
        case Logical(r) =>
          val topRows = topSublayout.numRows(s)
          polarity match
            case Polarity.+ =>
              if r <= topRows then topSublayout.tipY(s, Logical(r))
              else bottomOffset + bottomSublayout.tipY(s, Logical(r - topRows))
            case Polarity.- =>
              if r == 1 then topSublayout.tipY(s, Logical(topRows))
              else bottomOffset + bottomSublayout.tipY(s, Logical(r - 1))
