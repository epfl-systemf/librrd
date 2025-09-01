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

enum Polarity { case +, - }

enum TipSpecification:
  case Vertical
  case Physical(p: Double)
  case Logical(r: Int)

case class SidedProperty[T](left: T, right: T):
  def apply(s: Side) = s match { case Side.Left => left; case _ => right }
  def update(s: Side, v: T) = s match
    case Side.Left => SidedProperty(v, right)
    case _ => SidedProperty(left, v)
  def map[U](f: T => U) = SidedProperty(f(left), f(right))
trait SidedPropertyCommon:
  def forEach[U](f: Side => U) = SidedProperty(f(Side.Left), f(Side.Right))
trait SidedPropertyCompanion[T] extends SidedPropertyCommon:
  def apply(left: T, right: T) = SidedProperty(left, right)
object SidedProperty extends SidedPropertyCommon

type TipSpecifications = SidedProperty[TipSpecification]
object TipSpecifications extends SidedPropertyCompanion[TipSpecification]

type NumRows = SidedProperty[Int]
object NumRows extends SidedPropertyCompanion[Int]


def assertSingletonList[T](list: List[T]): T =
  assert(list.length == 1, "list must have exactly 1 element")
  list(0)

def splitEnds[T](seq: Seq[T]): (T, Seq[T], T) =
  val (firsts, rests) = seq.splitAt(1)
  val (mids, lasts) = rests.splitAt(rests.length - 1)
  (firsts(0), mids, lasts(0))

def trimSides[T, U](seq: Seq[T], trim: PartialFunction[T, U])
  : (SidedProperty[Option[U]], Seq[T]) =
  val maybeSides = SidedProperty(seq.headOption, seq.lastOption)
    .map[Option[U]](_.collect(trim))
  val withoutSides = seq
    .drop(if maybeSides.left.isDefined then 1 else 0)
    .dropRight(if maybeSides.right.isDefined then 1 else 0)
  (maybeSides, withoutSides)

type Subset[T] = List[T]
type Partition[T] = List[Subset[T]]
type PartitionIndices = List[Subset[Int]]

def allPartitions[T](l: List[T], startIndex: Int = 0): Vector[(Partition[T], PartitionIndices)] =
  l match
    case Nil => Vector((List(List()), List(List())))
    case elem :: Nil => Vector((List(List(elem)), List(List(startIndex))))
    case head :: tail => allPartitions(tail, startIndex + 1).flatMap((rp, rpi) =>
      List((List(head) +: rp, List(0) +: rpi),
           ((head +: rp.head) +: rp.tail, (0 +: rpi.head) +: rpi.tail)))


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
    if numItems == 0 then List(space) else {
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
    }


enum AlignItemsPolicy:
  case Top, Center, Bottom, Baseline
