package librrd

enum Direction:
  case LTR, RTL
  def reverse: Direction = this match
    case LTR => RTL
    case RTL => LTR
  def reverse[T](seq: Seq[T]): Seq[T] = this match
    case LTR => seq
    case RTL => seq.reverse
  def swap[T](t: (T, T)) = this match
    case LTR => t
    case RTL => t.swap

enum Side { case Left, Right }
enum RelativeSide:
  case Start, End
  def absolute(dir: Direction): Side =
    (this, dir) match
      case (Start, Direction.LTR) | (End, Direction.RTL) => Side.Left
      case (End, Direction.LTR) | (Start, Direction.RTL) => Side.Right

enum Polarity { case +, - }

enum TipSpecification:
  case Vertical
  case Physical(p: Double)
  case Logical(r: Int)

case class SidedProperty[T](left: T, right: T):
  def apply(s: Side) = s match { case Side.Left => left; case _ => right }
  def map[U](f: T => U) = SidedProperty(f(left), f(right))
trait SidedPropertyCompanion[T]:
  def apply(left: T, right: T) = SidedProperty(left, right)
  def forEach[U](f: Side => U) = SidedProperty(f(Side.Left), f(Side.Right))

type TipSpecifications = SidedProperty[TipSpecification]
object TipSpecifications extends SidedPropertyCompanion[TipSpecification]:
  val default = TipSpecifications(TipSpecification.Logical(1), TipSpecification.Logical(1))

case class RelativeTipSpecifications(start: TipSpecification, end: TipSpecification):
  import RelativeSide.*
  def get(s: RelativeSide) = s match { case Start => start; case End => end }
  def toAbsolute(direction: Direction) =
    TipSpecifications.apply.tupled(direction.swap((start, end)))

def assertSingletonList[T](list: List[T]): T =
  assert(list.length == 1, "list must have exactly 1 element")
  list(0)

val TOLERANCE = 0.0001
extension (self: Double)
  def ~=(other: Double): Boolean =
    Math.abs(self - other) < TOLERANCE
  def ~<=(other: Double): Boolean =
    self - other < TOLERANCE
  def ~>=(other: Double): Boolean =
    other - self < TOLERANCE


val MIN_GAP = 0.0

enum JustifyContentPolicy:
  case Start, Left, Right, End
  case SpaceBetween, SpaceAround, SpaceEvenly, Center

  import Direction.*

  def flush(s: Side, d: Direction): Boolean = (this, s, d) match
    case (SpaceBetween, _, _) => true
    case (Left, Side.Left, _) | (Right, Side.Right, _) => true
    case (Start, Side.Left, LTR) | (End, Side.Right, LTR) => true
    case (Start, Side.Right, RTL) | (End, Side.Left, RTL) => true
    case _ => false

  def distribute(space: Double, numItems: Int, direction: Direction): List[Double] =
    assert(space ~>= (numItems - 1)*MIN_GAP,
      "space to justify must be at least MIN_GAP*(N-1)")
    val extra = space - (numItems - 1)*MIN_GAP
    val distributed = (this, direction) match
      case (Left, _) | (Start, LTR) | (End, RTL) =>
        0.0 +: List.fill(numItems - 1)(MIN_GAP) :+ extra
      case (Right, _) | (Start, RTL) | (End, LTR) =>
        extra +: List.fill(numItems - 1)(MIN_GAP) :+ 0.0
      case (SpaceBetween, _) =>
        if numItems == 1 then List(space/2, space/2)
        else 0.0 +: List.fill(numItems - 1)(space/(numItems - 1)) :+ 0.0
      case (SpaceAround, _) =>
        val inner = Math.max(MIN_GAP, space/numItems)
        val outer = (space - inner*(numItems - 1))/2
        outer +: List.fill(numItems - 1)(inner) :+ outer
      case (SpaceEvenly, _) =>
        val inner = Math.max(MIN_GAP, space/(numItems + 1))
        val outer = (space - inner*(numItems - 1))/2
        outer +: List.fill(numItems - 1)(inner) :+ outer
      case (Center, _) =>
        extra/2 +: List.fill(numItems - 1)(MIN_GAP) :+ extra/2

    assert((distributed.length == numItems + 1)
        && (distributed.sum ~= space)
        && (distributed.drop(1).dropRight(1).forall(MIN_GAP ~<= _)),
      "justify-content implementation error")
    distributed


enum AlignItemsPolicy:
  import TipSpecification.*
  import Diagrams.*
  case Top, Center, Bottom, Baseline
  def defaultTipSpecification
      (diagram: Diagram, JCPolicy: JustifyContentPolicy, direction: Direction)
      (side: Side): TipSpecification =
    (this, diagram) match
      case (_, _: (TerminalToken | NonterminalToken)) => Logical(1)
      case (Top, _) => Physical(0)
      case (Bottom, _) => Physical(1)
      case (Baseline, stack: Stack)
          if stack.topSubdiagram == Sequence(Seq.empty)
            /* && numRows(stack.bottomSubdiagram, JCPolicy, direction, side) == 1 */ =>
        Logical(2)
      case (_, stack: Stack) =>
       Logical(/*numRows(stack, JCPolicy, direction, side)/2 + 1*/ 1)
      case (Baseline, sequence: Sequence) =>
        if JCPolicy.flush(side, direction)
        then Baseline.defaultTipSpecification
          (sidemost(sequence, side, direction), JCPolicy, direction)(side)
        else Logical(1)
      case (Center, _: Sequence) => Physical(0.5)
