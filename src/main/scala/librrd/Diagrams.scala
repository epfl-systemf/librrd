package librrd

object Diagrams:

  trait DiagramFields:
    val id: Option[String]
    val classes: Set[String]
    def toDiagram: Diagram


  sealed trait Diagram extends DiagramFields:
    def toDiagram = this

  case class TerminalToken(label: String,
                           classes: Set[String] = Set.empty,
                           id: Option[String] = None) extends Diagram
  case class NonterminalToken(label: String,
                              classes: Set[String] = Set.empty,
                              id: Option[String] = None) extends Diagram
  case class Sequence(subdiagrams: Seq[Diagram],
                      classes: Set[String] = Set.empty,
                      id: Option[String] = None) extends Diagram
  case class Stack(topSubdiagram: Diagram,
                   bottomSubdiagram: Diagram,
                   polarity: Polarity,
                   classes: Set[String] = Set.empty,
                   id: Option[String] = None) extends Diagram

  def sidemost(sequence: Sequence, side: Side, direction: Direction): Diagram =
    (side, direction) match
      case (Side.Left, Direction.LTR) | (Side.Right, Direction.RTL) =>
        sequence.subdiagrams.head
      case (Side.Right, Direction.LTR) | (Side.Left, Direction.RTL) =>
        sequence.subdiagrams.last
