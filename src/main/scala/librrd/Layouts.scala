package librrd

trait Layouts[T]:

  def measure(text: String): (Double, Double)
  def render(layout: Layout): T

  private var lastID = 0
  val generatedIDPrefix = "librrd-generated-"

  def freshID(): String =
    lastID += 1
    generatedIDPrefix + lastID.toString()

  def resetID(): Unit = lastID = 0


  enum Direction { case LTR, RTL }
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

  import Side.*
  import TipSpecification.*


  sealed trait Layout:
    val direction: Direction
    val id: String
    def width: Double
    def height: Double
    def classes: Seq[String]
    val tipSpec: Side => TipSpecification
    def tipHeight(s: Side, ts: TipSpecification): Double
    def tipHeight(s: Side): Double = tipHeight(s, tipSpec(s))

  object Layout:
    val unitWidth = 4
    val `class` = "librrd"


  trait InlineLayout extends Layout:
    val tipSpec = Map(Left -> Logical(0), Right -> Logical(0))


  object Rail:
    val `class` = "librrd-rail"

  case class Rail(
      val width: Double,
      val direction: Direction,
      initClasses: Seq[String] = Seq.empty,
      val id: String = freshID()) extends InlineLayout:
    val classes = initClasses :+ Rail.`class`
    val height = 0
    def tipHeight(s: Side, ts: TipSpecification) = 0


  object Space:
    val `class` = "librrd-space"

  case class Space(
      val width: Double,
      val direction: Direction,
      initClasses: Seq[String] = Seq.empty,
      val id: String = freshID()) extends InlineLayout:
    val classes = initClasses :+ Space.`class`
    val height = 0
    def tipHeight(s: Side, ts: TipSpecification) = 0


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
      initClasses: Seq[String] = Seq.empty,
      val id: String = freshID()) extends InlineLayout:
    val (textWidth, textHeight) = measure(label)
    val width = textWidth + 4*Station.paddingX
    val height = textHeight + 2*Station.paddingY

    val classes = initClasses
      :+ Station.`class`
      :+ (if isTerminal then Station.terminalClass else Station.nonterminalClass)

    def tipHeight(s: Side, ts: TipSpecification) = height/2


  object HorizontalConcatenation:
    val `class` = "librrd-hconcat"

  case class HorizontalConcatenation(
      val sublayouts: Seq[Layout],
      initClasses: Seq[String] = Seq.empty,
      val id: String = freshID()) extends InlineLayout:

    val width = sublayouts.map(_.width).sum
    val height = ???
    val classes = initClasses :+ HorizontalConcatenation.`class`

    assert(!sublayouts.isEmpty, "horizontal concatenation must have at least 1 sublayout")
    assert(
      sublayouts.forall(_.direction == sublayouts(0).direction),
      "sublayouts of horizontal concatenation must all have same direction")
    val direction = sublayouts(0).direction

    def sidemost(s: Side) = s match
      case Left => sublayouts.head
      case Right => sublayouts.last

    def tipHeight(s: Side, ts: TipSpecification) = sidemost(s).tipHeight(s, ts)
