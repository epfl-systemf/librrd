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
    val numRows: Side => Int
    val tipSpecs: TipSpecifications

    final def tipY(s: Side, ts: TipSpecification): Double =
      ts match
        case Vertical => ()
        case Physical(p) =>
          assert(0 <= p && p <= 1,
            "if tip specification is physical, proportion must be between 0 and 1")
        case Logical(r) =>
          assert(1 <= r && r <= numRows(s),
            "if tip specification is logical, row number must be between 1 and "
            + "number of rows")
      tipYInner(s, ts)

    def tipYInner(s: Side, ts: TipSpecification): Double
    def tipY(s: Side): Double = tipY(s, tipSpecs(s))

  object Layout:
    val unitWidth = 4.0
    val `class` = "librrd"
    val rowGap = 2*unitWidth


  trait InlineLayout:
    val numRows: Side => Int = { case _ => 1 }
    val tipSpecs = TipSpecifications(Logical(1), Logical(1))


  object Rail:
    val `class` = "librrd-rail"

  case class Rail(
      val width: Double,
      val direction: Direction,
      initClasses: Set[String] = Set.empty,
      initId: Option[String] = None) extends Layout with InlineLayout:
    val classes = initClasses + Rail.`class`
    val id = initId.getOrElse(freshID())
    val height = 0
    def tipYInner(s: Side, ts: TipSpecification) = 0


  object Space:
    val `class` = "librrd-space"

  case class Space(
      val width: Double,
      val direction: Direction,
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
      val label: String,
      val isTerminal: Boolean,
      val direction: Direction,
      initClasses: Set[String] = Set.empty,
      initId: Option[String] = None) extends Layout with InlineLayout:
    val (textWidth, textHeight) = measure(label)
    val width = textWidth + 4*Station.paddingX
    val height = textHeight + 2*Station.paddingY

    val classes = initClasses
    val id = initId.getOrElse(freshID())
      + Station.`class`
      + (if isTerminal then Station.terminalClass else Station.nonterminalClass)

    def tipYInner(s: Side, ts: TipSpecification) = height/2


  object HorizontalConcatenation:
    val `class` = "librrd-hconcat"

  case class HorizontalConcatenation(
      val sublayouts: Seq[Layout],
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
      sublayouts.foldRight((rightTipY, List(0.0))){ (sub, acc) => acc match
        case (prevLeftTipY, prevYs) =>
          val subY = prevLeftTipY - sub.tipY(Right)
          (subY + sub.tipY(Left), subY :: prevYs)
      }

    def tipYInner(s: Side, ts: TipSpecification) =
      (s match
        case Left => sublayouts.head
        case Right => sublayouts.last)
      .tipY(s, ts)

    override def tipY(s: Side) = s match
      case Left => leftTipY
      case Right => rightTipY


  object InlineVerticalConcatenation:
    val `class` = "librrd-vconcat-inline"
    val markerPadding = Layout.unitWidth

  case class InlineVerticalConcatenation(
      val sublayouts: Seq[Layout],
      val marker: String,
      val tipSpecs: TipSpecifications,
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
    val innerWidth = sublayouts.head.width + markerWidth
    val width = innerWidth
      + (tipSpecs(startSide) match
          case Physical(p) if p != 0 => 3*Layout.unitWidth
          case _ => 0)
      + (tipSpecs(endSide) match
          case Physical(p) if p != 1 => 3*Layout.unitWidth
          case _ => 0)
    assert(sublayouts.head.width == sublayouts.last.width,
      "first and last sublayout of inline vertical concatenation must have same width")
    assert(sublayouts.drop(1).dropRight(1).forall(_.width + 2*markerWidth == innerWidth),
      "middle sublayouts of inline vertical concatenation must all have width "
      + "equal to the first minus the marker width")

    val height = sublayouts.map(_.height).sum + Layout.rowGap * (sublayouts.length - 1)
    val numRows: Side => Int = { s =>
      (if s == startSide then sublayouts.head else sublayouts.last).numRows(s)
    }

    override def tipYInner(s: Side, ts: TipSpecification): Double =
      ts match
        case Vertical => Double.NaN
        case Physical(p) =>
          linearInterpolate(0, sublayouts.head.tipY(startSide, Physical(0)),
                            1, sublayouts.last.tipY(endSide, Physical(1)), p)
        case Logical(r) =>
          if s == startSide then
            sublayouts.head.tipY(s, ts)
          else
            height - sublayouts.last.height + sublayouts.last.tipY(s, ts)
