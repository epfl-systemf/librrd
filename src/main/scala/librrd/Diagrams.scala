package librrd

enum Polarity { case +, - }

enum Diagram(val numRows: RelativeSide => Int):
  case TerminalToken(label: String) extends Diagram(_ => 1)
  case NonterminalToken(label: String) extends Diagram(_ => 1)
  case Sequence(subdiagrams: Seq[Diagram])
    extends Diagram({
      case RelativeSide.Start => subdiagrams.head.numRows(RelativeSide.Start)
      case RelativeSide.End => subdiagrams.last.numRows(RelativeSide.End)
    })
  case Stack(topSubdiagram: Diagram,
             bottomSubdiagram: Diagram,
             polarity: Polarity)
    extends Diagram(rs =>
      (polarity match
        case Polarity.+ => topSubdiagram.numRows(rs)
        case Polarity.- => 1) + bottomSubdiagram.numRows(rs))


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
