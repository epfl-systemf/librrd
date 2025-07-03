package librrd

enum Direction:
  case LTR, RTL
  def reverse: Direction = this match
    case LTR => RTL
    case RTL => LTR
  def reverse[T](list: List[T]): List[T] = this match
    case LTR => list
    case RTL => list.reverse

enum Side { case Left, Right }
enum RelativeSide:
  case Start, End
  def absolute(dir: Direction): Side =
    (this, dir) match
      case (Start, Direction.LTR) | (End, Direction.RTL) => Side.Left
      case (End, Direction.LTR) | (Start, Direction.RTL) => Side.Right

enum TipSpecification:
  case Vertical
  case Physical(p: Double)
  case Logical(r: Int)

def absoluteTipSpec(relativeTipSpec: RelativeSide => TipSpecification, direction: Direction)
    : Side => TipSpecification =
  import RelativeSide.*
  s => if s == Start.absolute(direction) then relativeTipSpec(Start) else relativeTipSpec(End)

def assertSingletonList[T](list: List[T]): T =
  assert(list.length == 1, "list must have exactly 1 element")
  list(0)
