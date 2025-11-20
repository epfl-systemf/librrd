package librrd

import scala.util.{Either, Left, Right}
import Side.*
import TipSpecification.*

trait SBlockLayouts[T]:

  def measure(text: String, font: FontInfo): (Double, Double)
  def render(layout: Layout): T

  object Layout:
    val unitWidth = 4.0
    val `class` = "librrd"
    val rowGap = 3*unitWidth

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

    def tipYInternal(s: Side, ts: TipSpecification): Double
    def tipYInternal: SidedProperty[Double] = SidedProperty.forEach(s => tipYInternal(s, tipSpecs(s)))

    def block: BlockLayout


  sealed trait BlockLayout extends Layout:
    val sBlockRows = 1
    val startOffset = 0
    def endWidth = width
    def endHeight = startHeight
    def middleHeight = -startHeight
    override def block = this

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
    val startHeight = 2*Layout.unitWidth
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

    val middleHeight = 2*Layout.rowGap

    val sBlockRows = 2
    override def growStartHeight(by: Double) = this.copy(startHeight = startHeight + by)
    override def growEndHeight(by: Double) = this.copy(endHeight = endHeight + by)

    val tipRowsPossible = NumRows(1, 1)
    val tipSpecs = TipSpecifications(Logical(1), Logical(1))
    override val tipYInternal = SidedProperty(direction, 0, endHeight)
    def tipYInternal(s: Side, ts: TipSpecification): Double = ts match
      case Vertical => throw IllegalArgumentException("line breaks cannot have vertical tips")
      case _ => tipYInternal(s)

    override def block = throw IllegalArgumentException("line breaks cannot be blocked")


  object HorizontalConcatenation:
    val `class` = "librrd-hconcat"
    val blockedClass = "librrd-hconcat-blocked"

    def adjustHeights(sublayouts: Seq[Layout]): Seq[Layout] =
      val (startSide, endSide) = sublayouts(0).direction.swap((Side.Left, Side.Right))
      def loop(prevEndHeight: Double, prevEndTipY: Double, prevSBlockRows: Int,
               subs: Seq[Layout]): Either[Double, Seq[Layout]] =
        subs match
          case Nil => Right(Nil)
          case sub :: rest =>
            val topJoinHeightDiff = sub.tipYInternal(startSide) - prevEndTipY
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
              val endTipY = maybeStartGrownSub.tipYInternal(endSide)
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
      loop(head.startHeight, head.tipYInternal(startSide), 1, sublayouts) match
        case Right(result) => result
        case Left(_) => throw RuntimeException(s"could not adjust heights of sequence! $sublayouts")


  case class HorizontalConcatenation(
      sublayouts: Seq[Layout],
      initClasses: Set[String] = Set.empty,
      id: Option[String] = None) extends Layout:
    val classes = initClasses + HorizontalConcatenation.`class`

    assert(!sublayouts.isEmpty, "horizontal concatenation must have at least 1 sublayout")
    assert(
      sublayouts.forall(_.direction == sublayouts(0).direction),
      "sublayouts of horizontal concatenation must all have same direction")
    val direction = sublayouts(0).direction
    private val first = sublayouts.head
    private val last = sublayouts.last // could be same as first!
    val sidemosts = SidedProperty(direction, first, last)
    val (startSide, endSide) = direction.swap((Side.Left, Side.Right))

    private case class HCMeasure(
      startOffset: Double, endWidth: Double, width: Double, sBlockRows: Int,
      startHeight: Double, startTipY: Double, middleHeight: Double,
      endHeight: Double, endTipY: Double)
    private object HCMeasure:
      def apply(l: Layout): HCMeasure =
        HCMeasure(l.startOffset, l.endWidth, l.width, l.sBlockRows,
                  l.startHeight, l.tipYInternal(startSide), l.middleHeight,
                  l.endHeight, l.tipYInternal(endSide))

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
          (so, ew, w, sbr, sh, sty, mh, eh, ety)

    val (subXs, subYs) =
      val (tempXs, tempYs) = sublayouts.scanLeft((0.0, first.tipYInternal(startSide))){
        case ((prevX, prevY), sub) =>
          (prevX - sub.startOffset + sub.endWidth,
           prevY - sub.tipYInternal(startSide) + sub.tipY(endSide))
        }.dropRight(1)
         .zip(sublayouts)
         .map{ case ((x, y), sub) =>
           val startX = x - sub.startOffset
           ((direction match { case Direction.LTR => startX; case _ => -(startX + sub.width) }),
            y - sub.tipYInternal(startSide)) }
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

    private def physicalTip0 = startTipY + tipSpecOffsets(startSide)
      - first.tipYInternal(startSide) + first.tipYInternal(startSide, Physical(0))
    private def physicalTip1 = endTipY + tipSpecOffsets(endSide)
      - last.tipYInternal(endSide) + last.tipYInternal(endSide, Physical(1))
    private def physicalTipY(p: Double) =
      linearInterpolate(0, physicalTip0, 1, physicalTip1, p)

    def tipYInternal(s: Side, ts: TipSpecification) =
      ts match
        case Vertical => tipYInternal(s)
        case Physical(p) => physicalTipY(p)
        case Logical(r) =>
          tipYInternal(s) - sidemosts(s).tipYInternal(s) + sidemosts(s).tipYInternal(s, ts)

    override val tipYInternal = SidedProperty(direction, startTipY, endTipY)
    override val tipSpecs = SidedProperty.forEach(s => sidemosts(s).tipSpecs(s))
    val tipRowsPossible = NumRows(direction, first.tipRows, last.tipRows)

    override def block =
      BlockedHorizontalConcatenation(this)

    val endsWithVerticalEpsilon: Boolean = last match
      case l: HorizontalConcatenation => l.endsWithVerticalEpsilon
      case _: VerticalEpsilon => true
      case _ => false


  object BlockedHorizontalConcatenation:
    def extraWidths(direction: Direction, tipSpecs: TipSpecifications): SidedProperty[Double] =
      val extraP = SidedProperty(direction, 0, 1)
      SidedProperty.forEach(s => tipSpecs(s) match
        case Physical(p) if p != extraP(s) => 3*Layout.unitWidth
        case Logical(_) => 2*Layout.unitWidth
        case _ => 0)


  case class BlockedHorizontalConcatenation(
      hc: HorizontalConcatenation,
      maybeTipSpecs: Option[TipSpecifications] = None) extends BlockLayout:

    assert(hc.startOffset ~= 0,
      s"blocked horizontal concatenation startOffset should be 0, got ${hc.startOffset}")
    assert(hc.endWidth ~= hc.width,
      s"blocked horizontal concatenation endWidth should equal width=${hc.width}, got ${hc.endWidth}")

    override val direction = hc.direction
    override val id = hc.id
    override val classes = hc.classes + HorizontalConcatenation.blockedClass
    override val startHeight = hc.height
    override val tipSpecs = maybeTipSpecs.getOrElse(hc.tipSpecs)
    val extraWidths = BlockedHorizontalConcatenation.extraWidths(direction, tipSpecs)
    override val width = hc.width + extraWidths.left + extraWidths.right
    override val tipRowsPossible = hc.tipRowsPossible

    override def tipYInternal(s: Side, ts: TipSpecification) = ts match
      case Vertical =>
        hc.tipY(s, ts)
      case Physical(p) =>
        assert(p == SidedProperty(direction, 0, 1)(s) || hc.tipSpecs(s) == Vertical)
        hc.tipYInternal(s, ts) // don't relativize to hc.endHeight
      case Logical(r) =>
        hc.tipY(s, ts)


  object VerticalConcatenation:
    val `class` = "librrd-vconcat"
    val positiveClass = "librrd-vconcat-positive"
    val negativeClass = "librrd-vconcat-negative"

    def extraWidths(tipSpecs: TipSpecifications): SidedProperty[Double] = SidedProperty.forEach(s =>
      if tipSpecs(s) == TipSpecification.Vertical then 0 else 3*Layout.unitWidth)

  case class VerticalConcatenation(
      topSublayout: Layout,
      bottomSublayout: Layout,
      direction: Direction,
      polarity: Polarity,
      tipSpecs: TipSpecifications,
      initClasses: Set[String] = Set.empty,
      id: Option[String] = None) extends BlockLayout:
    val `classes` = initClasses
      + VerticalConcatenation.`class`
      + (polarity match
           case Polarity.+ => VerticalConcatenation.positiveClass
           case Polarity.- => VerticalConcatenation.negativeClass)

    assert(topSublayout.width ~= bottomSublayout.width,
      "top and bottom sublayouts of vertical concatenation must have same width " +
      s"${topSublayout.width} ${bottomSublayout.width}!")
    val extraWidths = VerticalConcatenation.extraWidths(tipSpecs)
    val width = topSublayout.width + extraWidths.left + extraWidths.right

    assert(topSublayout.direction == (polarity match
        case Polarity.+ =>  bottomSublayout.direction
        case Polarity.- =>  bottomSublayout.direction.reverse),
      "bottom sublayout of vertical concatenation must have direction equal to " +
      "top sublayout iff polarity is positive!")
    val bottomOffset = topSublayout.height + Layout.rowGap
    val startHeight = bottomOffset + bottomSublayout.height

    assert(Side.values.forall(s => topSublayout.tipSpecs(s) == Vertical)
      && Side.values.forall(s => bottomSublayout.tipSpecs(s) == Vertical),
      "top and bottom sublayouts of vertical concatenation must have vertical tips!")
    assert(topSublayout.isInstanceOf[BlockLayout] && bottomSublayout.isInstanceOf[BlockLayout],
      "top and bottom sublayouts of vertical concatenation must be block layouts!")

    override val tipRowsPossible =
      polarity match
        case Polarity.+ => NumRows.forEach(s =>
          topSublayout.tipRows(s) + bottomSublayout.tipRows(s))
        case Polarity.- => NumRows.forEach(s =>
          topSublayout.tipRowsPossible(s) + bottomSublayout.tipRowsPossible(s)
          - topSublayout.tipRows(s) + 1)

    override val tipRows = NumRows.forEach(s => (tipSpecs(s), polarity) match
      case (Vertical, Polarity.+) => tipRowsPossible(s)
      case _ => 1)

    override def tipYInternal(s: Side, ts: TipSpecification) = ts match
      case Vertical => tipYInternal(s, Logical(1))
      case Physical(p) => linearInterpolate(
          0, topSublayout.tipY(s, polarity match
            case Polarity.+ => Physical(0)
            case Polarity.- => Logical(topSublayout.tipRowsPossible(s))),
          1, bottomOffset + bottomSublayout.tipY(s, Physical(1)), p)
        case Logical(r) =>
          val topRows = topSublayout.tipRows(s)
          polarity match
            case Polarity.+ =>
              if r <= topRows then topSublayout.tipY(s, Logical(r))
              else bottomOffset + bottomSublayout.tipY(s, Logical(r - topRows))
            case Polarity.- =>
              if r == 1 then topSublayout.tipY(s, Logical(topRows))
              else bottomOffset + bottomSublayout.tipY(s, Logical(r - 1))


  object VerticalEpsilon:
    val `class` = "librrd-vepsilon"

    def extraWidth(polarity: Polarity, startTipSpec: TipSpecification): Double =
      startTipSpec match { case Vertical => 0 ; case _ => 3*Layout.unitWidth }

  case class VerticalEpsilon(
      sub: Layout,
      width: Double,
      direction: Direction,
      polarity: Polarity,
      tipSpecs: TipSpecifications,
      extraStartHeight: Double = 0.0,
      extraEndHeight: Double = 0.0,
      initClasses: Set[String] = Set.empty,
      id: Option[String] = None) extends Layout:
    val `classes` = initClasses + VerticalEpsilon.`class`

    val (startSide, endSide) = direction.swap((Side.Left, Side.Right))
    assert(sub.direction == direction, "vertical epsilon sub must have same direction")

    sub match
      case _: BlockLayout =>
        assert(Side.values.forall(s => sub.tipSpecs(s) == Vertical),
          "vertical epsilon sub, if block, must have Vertical tips")
      case hc: HorizontalConcatenation =>
        assert(hc.endsWithVerticalEpsilon && hc.tipSpecs(startSide) == Vertical,
          "vertical epsilon sub, if horizontal concatenation, must end with vertical epsilon "
          + "and have Vertical start tip")
      case ve: VerticalEpsilon =>
        assert(ve.tipSpecs(startSide) match { case Logical(_) => true ; case _ => false },
          "vertical epsilon sub, if vertical epsilon, must have Logical start tip ")
      case _ => assert(false, s"vertical epsilon cannot have $sub as sub")

    val extraWidth = VerticalEpsilon.extraWidth(polarity, tipSpecs(startSide))
    sub match
      case _: BlockLayout => assert(width ~>= sub.width + extraWidth,
        s"vertical epsilon with block sub must have width $width no less than sum of "
        + s"sub width ${sub.width} and extra width $extraWidth!")
      case _ => assert(width ~= sub.width + extraWidth,
        s"vertical epsilon with non-block sub must have width $width equal to sum of "
        + s"sub width ${sub.width} and extra width $extraWidth!")
    override val startOffset = width - sub.width - extraWidth
    override val endWidth =
      tipSpecs(startSide) match { case Logical(_) if (startOffset ~= 0) => extraWidth; case _ => 0 }

    override val (startHeight, middleHeight, endHeight) = sub match
      case _: BlockLayout => (sub.height + extraStartHeight, 2*Layout.rowGap, extraEndHeight)
      case _ => (sub.startHeight, sub.middleHeight, sub.endHeight)

    val sBlockRows = 2
    override val startHeightGrowable = true
    override def growStartHeight(by: Double) = sub match
      case _: BlockLayout => this.copy(extraStartHeight = extraStartHeight + by)
      case _ => this.copy(sub = sub.growStartHeight(by))
    override val endHeightGrowable = true
    override def growEndHeight(by: Double) = sub match
      case _: BlockLayout => this.copy(extraEndHeight = extraEndHeight + by)
      case _ => this.copy(sub = sub.growEndHeight(by))

    override val tipRowsPossible = NumRows(direction,
      polarity match { case Polarity.+ => sub.tipRows(startSide) case Polarity.- => 1 },
      1)

    assert(tipSpecs(endSide) == Logical(1), "vertical epsilon end side tip spec must be Logical(1)")

    override def tipYInternal(s: Side, ts: TipSpecification): Double =
      if s == startSide then
        (ts, polarity) match
          case (Logical(r), Polarity.-) => sub.tipYInternal(s, Logical(sub.tipRows(s)))
          case _ => sub.tipYInternal(s, ts)
      else endHeight

    override def block = throw IllegalArgumentException("line breaks cannot be blocked")
