package librrd

enum Polarity { case +, - }

enum Diagram:
  val id: Option[String]
  val classes: Seq[String]

  case TerminalToken(label: String,
                     classes: Seq[String] = Seq.empty,
                     id: Option[String] = None)
  case NonterminalToken(label: String,
                        classes: Seq[String] = Seq.empty,
                        id: Option[String] = None)
  case Sequence(subdiagrams: Seq[Diagram],
                classes: Seq[String] = Seq.empty,
                id: Option[String] = None)
  case Stack(topSubdiagram: Diagram,
             bottomSubdiagram: Diagram,
             polarity: Polarity,
             classes: Seq[String] = Seq.empty,
             id: Option[String] = None)

object Diagram:
  def sidemost(sequence: Sequence, side: Side, direction: Direction): Diagram =
    (side, direction) match
      case (Side.Left, Direction.LTR) | (Side.Right, Direction.RTL) =>
        sequence.subdiagrams.head
      case (Side.Right, Direction.LTR) | (Side.Left, Direction.RTL) =>
        sequence.subdiagrams.last


enum AlignedDiagram:
  val direction: Direction

  case Station(direction: Direction,
               isTerminal: Boolean,
               label: String)
  case Space(direction: Direction)
  case Sequence(direction: Direction,
                tipSpec: Side => TipSpecification,
                subdiagrams: Seq[AlignedDiagram])
  case BlockVerticalConcatenation(
    direction: Direction,
    tipSpec: Side => TipSpecification,
    polarity: Polarity,
    topSubdiagram: AlignedDiagram,
    bottomSubdiagram: AlignedDiagram)
