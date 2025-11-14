package librrd

import scala.util.{Either, Left, Right}
import Side.*
import TipSpecification.*

trait SBlockLayouts[T]:

  def measure(text: String, font: FontInfo): (Double, Double)
  def render(layout: Layout): T

  enum RelativeSide { case Start, End }

  object Layout:
    val unitWidth = 4.0
    val `class` = "librrd"
    val rowGap = 4*unitWidth

  sealed trait Layout:
    def direction: Direction
    def id: Option[String]
    def classes: Set[String]

    def width: Double
    def startOffset: Double     // must be in [0, width]
    def endWidth: Double        // must be in [0, width]

    def startHeight: Double
    def middleHeight: Double
    def endHeight: Double
    def height: Double = startHeight + middleHeight + endHeight

    def sBlockRows: Int
    def startHeightGrowable: Boolean = sBlockRows > 1
    def growStartHeight(by: Double): Layout = this
    def endHeightGrowable: Boolean = sBlockRows > 1
    def growEndHeight(by: Double): Layout = this

    def tipRowsPossible: NumRows
    def tipSpecOffsets = SidedProperty(direction, 0.0, startHeight + middleHeight)
    def tipSpecs: TipSpecifications
    def tipRows: NumRows =
      NumRows.forEach(s => tipSpecs(s) match { case Vertical => tipRowsPossible(s); case _ => 1 })

    final def tipY(s: Side, ts: TipSpecification): Double =
      ts match
        case Vertical => ()
        case Physical(p) =>
          assert(0 <= p && p <= 1,
            "if tip specification is physical, proportion must be between 0 and 1")
        case Logical(r) =>
          assert(1 <= r && r <= tipRowsPossible(s),
            "if tip specification is logical, row number must be between 1 and tipRowsInternal="
            + tipRowsPossible(s).toString())
      tipSpecOffsets(s) + tipYInternal(s, ts)

    final def tipY: SidedProperty[Double] =
      SidedProperty.forEach(s => tipSpecOffsets(s) + tipYInternal(s))

    final def tipY(rs: RelativeSide): Double =
      tipY((rs, direction) match
        case (RelativeSide.Start, Direction.LTR) | (RelativeSide.End, Direction.RTL) => Side.Left
        case _ => Side.Right)

    def tipYInternal(s: Side, ts: TipSpecification): Double
    def tipYInternal: SidedProperty[Double] = SidedProperty.forEach(s => tipYInternal(s, tipSpecs(s)))
    def tipYInternal(rs: RelativeSide): Double =
      tipYInternal((rs, direction) match
        case (RelativeSide.Start, Direction.LTR) | (RelativeSide.End, Direction.RTL) => Side.Left
        case _ => Side.Right)


  sealed trait BlockLayout extends Layout:
    val sBlockRows = 1
    val startOffset = 0
    def endWidth = width
    def endHeight = startHeight
    def middleHeight = -startHeight

  sealed trait AtomicLayout extends BlockLayout:
    val tipRowsPossible = NumRows(1, 1)
    val tipSpecs = TipSpecifications(Logical(1), Logical(1))


  object Rail:
    val `class` = "librrd-rail"

  case class Rail(
      width: Double,
      direction: Direction,
      initClasses: Set[String] = Set.empty,
      id: Option[String] = None) extends AtomicLayout:
    val classes = initClasses + Rail.`class`
    val startHeight = 0
    def tipYInternal(s: Side, ts: TipSpecification) = 0


  object Space:
    val `class` = "librrd-space"

  case class Space(
      width: Double,
      direction: Direction,
      verticalSide: Side,
      initClasses: Set[String] = Set.empty,
      id: Option[String] = None) extends AtomicLayout:
    val classes = initClasses + Space.`class`
    val startHeight = 0
    def tipYInternal(s: Side, ts: TipSpecification) = 0
    override val tipSpecs =
      TipSpecifications(Logical(1), Logical(1)).update(verticalSide, Vertical)


  object Station:
    val `class` = "librrd-station"
    val terminalClass = "librrd-terminal"
    val nonterminalClass = "librrd-nonterminal"

    val paddingX = Layout.unitWidth
    val paddingY = 1.5*Layout.unitWidth

  case class Station(
      label: String,
      isTerminal: Boolean,
      direction: Direction,
      font: FontInfo,
      initClasses: Set[String] = Set.empty,
      id: Option[String] = None) extends AtomicLayout:
    val classes = initClasses
      + Station.`class`
      + (if isTerminal then Station.terminalClass else Station.nonterminalClass)

    val (textWidth, textHeight) = measure(label, font)
    val width = textWidth + 4*Station.paddingX
    val startHeight = textHeight + 2*Station.paddingY
    def tipYInternal(s: Side, ts: TipSpecification) = height/2


  object LineBreak:
    val `class` = "librrd-linebreak"

  case class LineBreak(
      width: Double,
      direction: Direction,
      initClasses: Set[String] = Set.empty,
      id: Option[String] = None,
      startHeight: Double = 0.0,
      endHeight: Double = 0.0) extends Layout:
    val classes = initClasses + LineBreak.`class`

    val startOffset = width
    val endWidth = 0

    val middleHeight = Layout.rowGap

    val sBlockRows = 2
    override def growStartHeight(by: Double) = this.copy(startHeight = startHeight + by)
    override def growEndHeight(by: Double) = this.copy(endHeight = endHeight + by)

    val tipRowsPossible = NumRows(1, 1)
    val tipSpecs = TipSpecifications(Logical(1), Logical(1))
    override val tipYInternal = SidedProperty(direction, 0, endHeight)
    def tipYInternal(s: Side, ts: TipSpecification): Double = ts match
      case Vertical => throw IllegalArgumentException("line breaks cannot have vertical tips")
      case _ => tipYInternal(s)


  object HorizontalConcatenation:
    import RelativeSide.*
    val `class` = "librrd-hconcat"

    def adjustHeights(sublayouts: Seq[Layout]): Seq[Layout] =
      def loop(prevEndHeight: Double, prevEndTipY: Double, prevSBlockRows: Int,
               subs: Seq[Layout]): Either[Double, Seq[Layout]] =
        subs match
          case Nil => Right(Nil)
          case sub :: rest =>
            val topJoinHeightDiff = sub.tipYInternal(Start) - prevEndTipY
            val bottomJoinHeightDiff = sub.startHeight - prevEndHeight - topJoinHeightDiff
            if (prevSBlockRows > 1 && topJoinHeightDiff > 0) then
              Left(topJoinHeightDiff)
            else
              val maybeStartGrownSub =
                if (sub.sBlockRows > 1 && bottomJoinHeightDiff < 0) then
                  assert(sub.startHeightGrowable, s"start height of sub $sub must be growable!")
                  sub.growStartHeight(-bottomJoinHeightDiff)
                else sub

              val joinHeightDiff = Math.max(topJoinHeightDiff, 0) + Math.max(bottomJoinHeightDiff, 0)
              val joinHeight = prevEndHeight + joinHeightDiff
              val endHeight = if maybeStartGrownSub.sBlockRows == 1 then joinHeight
                              else maybeStartGrownSub.endHeight
              val endTipY = maybeStartGrownSub.tipYInternal(End)
                + (if maybeStartGrownSub.sBlockRows == 1 then Math.max(-topJoinHeightDiff, 0) else 0)
              val sBlockRows = prevSBlockRows + maybeStartGrownSub.sBlockRows - 1
              val maybeAdjustedRest = loop(endHeight, endTipY, sBlockRows, rest)

              maybeAdjustedRest match
                case Left(growBy) =>
                  if maybeStartGrownSub.endHeightGrowable then
                    loop(prevEndHeight, prevEndTipY, prevSBlockRows,
                         maybeStartGrownSub.growEndHeight(growBy) :: rest)
                  else Left(growBy)
                case Right(result) => Right(maybeStartGrownSub +: result)

      val head = sublayouts.head
      loop(head.endHeight, head.tipYInternal(End), 1, sublayouts) match
        case Right(result) => result
        case Left(_) => throw RuntimeException(s"could not adjust heights of sequence! $sublayouts")


  case class HorizontalConcatenation(
      sublayouts: Seq[Layout],
      tipSpecs: TipSpecifications,
      initClasses: Set[String] = Set.empty,
      id: Option[String] = None) extends Layout:
    import RelativeSide.*
    val classes = initClasses + HorizontalConcatenation.`class`

    assert(!sublayouts.isEmpty, "horizontal concatenation must have at least 1 sublayout")
    assert(
      sublayouts.forall(_.direction == sublayouts(0).direction),
      "sublayouts of horizontal concatenation must all have same direction")
    val direction = sublayouts(0).direction
    private val first = sublayouts.head
    private val last = sublayouts.last // could be same as first!
    private val sidemosts = SidedProperty(direction, first, last)

    Side.values.foreach(s => tipSpecs(s) match
      case Vertical => assert(sidemosts(s).tipSpecs(s) == Vertical)
      case Physical(p) => ()
      case Logical(r) =>
        assert(r == 1)
        assert(sidemosts(s).tipSpecs(s) != Vertical))

    private case class HCMeasure(
      startOffset: Double, endWidth: Double, width: Double, sBlockRows: Int,
      startHeight: Double, startTipY: Double, middleHeight: Double,
      endHeight: Double, endTipY: Double)
    private object HCMeasure:
      def apply(l: Layout): HCMeasure =
        HCMeasure(l.startOffset, l.endWidth, l.width, l.sBlockRows,
                  l.startHeight, l.tipYInternal(Start), l.middleHeight,
                  l.endHeight, l.tipYInternal(End))

    val (startOffset, endWidth, width, sBlockRows,
         startHeight, startTipY, middleHeight, endHeight, endTipY) =
      sublayouts.map(HCMeasure.apply).reduceLeft{ (before, after) =>
          val startWidthDiff = before.endWidth - after.startOffset
          val endWidthDiff = startWidthDiff + after.width - before.width

          val topJoinHeightDiff = after.startTipY - before.endTipY
          val bottomJoinHeightDiff = after.startHeight - before.endHeight - topJoinHeightDiff
          if before.sBlockRows > 1 then
            assert(topJoinHeightDiff <= 0, s"endHeight not tall enough before sub $after")
          if after.sBlockRows > 1 then
            assert(bottomJoinHeightDiff >= 0, s"startHeight not tall enough for sub $after")
          val joinHeightDiff = Math.max(topJoinHeightDiff, 0) + Math.max(bottomJoinHeightDiff, 0)
          val joinHeight = before.endHeight + joinHeightDiff

          HCMeasure(
/* startOffset  */ Math.max(-startWidthDiff, 0) + before.startOffset,
/* endWidth     */ Math.max(startWidthDiff, 0) + after.endWidth,
/* width        */ before.width + Math.max(-startWidthDiff, 0) + Math.max(endWidthDiff, 0),
/* sBlockRows   */ before.sBlockRows + after.sBlockRows - 1,
/* startHeight  */ (if before.sBlockRows == 1 then joinHeight else before.startHeight),
/* startTipY    */ before.startTipY
                     + (if before.sBlockRows == 1 then Math.max(topJoinHeightDiff, 0) else 0),
/* middleHeight */ ((before.sBlockRows, after.sBlockRows) match
                      case (1, 1) => -joinHeight
                      case (1, _) => after.middleHeight
                      case (_, 1) => before.middleHeight
                      case (_, _) => before.middleHeight + joinHeight + after.middleHeight),
/* endHeight    */ (if after.sBlockRows == 1 then joinHeight else after.endHeight),
/* endTipY      */ after.endTipY
                     + (if after.sBlockRows == 1 then Math.max(-topJoinHeightDiff, 0) else 0)
          )
      } match
        case HCMeasure(so, ew, w, sbr, sh, sty, mh, eh, ety) =>
          // TODO: adjust width for brackets
          (so, ew, w, sbr, sh, sty, mh, eh, ety)

    val (subXs, subYs) =
      val (tempXs, tempYs) = sublayouts.scanLeft((0.0, first.tipYInternal(Start))){
        case ((prevX, prevY), sub) =>
          (prevX - sub.startOffset + sub.endWidth,
           prevY - sub.tipYInternal(Start) + sub.tipY(End))
        }.dropRight(1)
         .zip(sublayouts)
         .map{ case ((x, y), sub) =>
           val startX = x - sub.startOffset
           (direction match { case Direction.LTR => startX; case _ => -(startX + sub.width) },
            y - sub.tipYInternal(Start)) }
         .unzip
      val minTempX = tempXs.min
      val minTempY = tempYs.min
      (tempXs.map(_ - minTempX), tempYs.map(_ - minTempY))

    assert(startOffset.~~(0, width))
    assert(endWidth.~~(0, width))
    assert(startTipY.~~(0, startHeight))
    assert(endTipY.~~(0, endHeight))
    if sBlockRows == 1 then assert((startHeight ~= endHeight) && (startHeight ~= -middleHeight))
    assert(width ~= sublayouts.map(_.width).zip(subXs).map((w, x) => w + x).max,
      s"horizontal concatenation width $width does not match maximum extent among sublayouts "
      + s"max(${sublayouts.map(_.width).zip(subXs).map((w, x) => s"$x + $w").mkString(", ")})")

    val firstStartGrowable = sublayouts.indexWhere(_.startHeightGrowable)
    override val startHeightGrowable =
      super.startHeightGrowable && sublayouts.take(firstStartGrowable).forall(_.sBlockRows == 1)
    override def growStartHeight(by: Double) = this.copy(sublayouts =
      sublayouts.updated(firstStartGrowable, sublayouts(firstStartGrowable).growStartHeight(by)))

    val lastEndGrowable = sublayouts.lastIndexWhere(_.endHeightGrowable)
    override val endHeightGrowable =
      super.endHeightGrowable && sublayouts.drop(lastEndGrowable + 1).forall(_.sBlockRows == 1)
    override def growEndHeight(by: Double) = this.copy(sublayouts =
      sublayouts.updated(lastEndGrowable, sublayouts(lastEndGrowable).growEndHeight(by)))

    override val tipYInternal = SidedProperty(direction, startTipY, endTipY)

    def tipYInternal(s: Side, ts: TipSpecification) =
      ts match
        case Vertical => 0.0
        case Physical(p) => linearInterpolate(
          0, sidemosts(s).tipYInternal(s, Physical(0)),
          1, sidemosts(s).tipYInternal(s, Physical(1)),
          p)
        case Logical(r) =>
          tipYInternal(s) - sidemosts(s).tipYInternal(s) + sidemosts(s).tipYInternal(s, ts)

    val tipRowsPossible = NumRows(direction, first.tipRows, last.tipRows)
