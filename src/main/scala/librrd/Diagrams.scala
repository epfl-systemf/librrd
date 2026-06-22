package librrd

object Diagrams:

  trait DiagramFields:
    val id: Option[String]
    val classes: Set[String]
    val groupLabel: Option[String]
    def toDiagram: Diagram


  sealed trait Diagram extends DiagramFields:
    def toDiagram = this
    def withMeta(classes: Set[String] = this.classes,
                 id: Option[String] = this.id,
                 groupLabel: Option[String] = this.groupLabel) =
      // TODO gross duplication but how else to do it?
      this match
        case d: TerminalToken => d.copy(groupLabel = groupLabel, id = id, classes = classes)
        case d: NonterminalToken => d.copy(groupLabel = groupLabel, id = id, classes = classes)
        case d: Sequence => d.copy(groupLabel = groupLabel, id = id, classes = classes)
        case d: Stack => d.copy(groupLabel = groupLabel, id = id, classes = classes)

  case class TerminalToken(label: String,
                           classes: Set[String] = Set.empty,
                           id: Option[String] = None,
                           groupLabel: Option[String] = None) extends Diagram
  case class NonterminalToken(label: String,
                              classes: Set[String] = Set.empty,
                              id: Option[String] = None,
                              groupLabel: Option[String] = None) extends Diagram
  case class Sequence(subdiagrams: Seq[Diagram],
                      classes: Set[String] = Set.empty,
                      id: Option[String] = None,
                      groupLabel: Option[String] = None) extends Diagram
  case class Stack(topSubdiagram: Diagram,
                   bottomSubdiagram: Diagram,
                   polarity: PrePolarity,
                   classes: Set[String] = Set.empty,
                   id: Option[String] = None,
                   groupLabel: Option[String] = None) extends Diagram

  def sidemost(sequence: Sequence, side: Side, direction: Direction): Diagram =
    (side, direction) match
      case (Side.Left, Direction.LTR) | (Side.Right, Direction.RTL) =>
        sequence.subdiagrams.head
      case (Side.Right, Direction.LTR) | (Side.Left, Direction.RTL) =>
        sequence.subdiagrams.last
