package librrd

import Direction.*

object DirectedDiagrams:

  trait DirectedDiagramFields extends ParameterizedDiagrams.ParameterizedDiagramFields:
    val direction: Direction
    def toDirectedDiagram: DirectedDiagram
    def toDiagram = toParameterizedDiagram.toDiagram

  sealed trait DirectedDiagram extends DirectedDiagramFields:
    def toDirectedDiagram = this

  case class Station(label: String,
                     isTerminal: Boolean,
                     direction: Direction,
                     properties: LayoutStylesheets.PropertyMap,
                     classes: Set[String] = Set.empty,
                     id: Option[String] = None,
                     groupLabel: Option[String] = None)
      extends DirectedDiagram:
    def toParameterizedDiagram =
      ParameterizedDiagrams.Station(label, isTerminal, properties, classes, id, groupLabel)

  case class Sequence(subdiagrams: Seq[DirectedDiagram],
                      direction: Direction,
                      properties: LayoutStylesheets.PropertyMap,
                      classes: Set[String] = Set.empty,
                      id: Option[String] = None,
                      groupLabel: Option[String] = None)
      extends DirectedDiagram:
    def toParameterizedDiagram =
      ParameterizedDiagrams.Sequence(subdiagrams.map(_.toParameterizedDiagram),
                                     properties, classes, id, groupLabel)

  case class Stack(topSubdiagram: DirectedDiagram,
                   bottomSubdiagram: DirectedDiagram,
                   direction: Direction,
                   polarity: PrePolarity,
                   properties: LayoutStylesheets.PropertyMap,
                   classes: Set[String] = Set.empty,
                   id: Option[String] = None,
                   groupLabel: Option[String] = None)
      extends DirectedDiagram:
    def toParameterizedDiagram =
      ParameterizedDiagrams.Stack(topSubdiagram.toParameterizedDiagram,
                                  bottomSubdiagram.toParameterizedDiagram,
                                  polarity, properties, classes, id, groupLabel)


  def direct(diagram: ParameterizedDiagrams.ParameterizedDiagram, direction: Direction = LTR)
      : DirectedDiagram =
    diagram match
      case ParameterizedDiagrams.Station(label, isTerminal, properties, classes, id, gLabel) =>
        Station(label, isTerminal, direction, properties, classes, id, gLabel)
      case ParameterizedDiagrams.Sequence(subdiagrams, properties, classes, id, gLabel) =>
        Sequence(direction.reverse(subdiagrams).map(direct(_, direction)),
          direction, properties, classes, id, gLabel)
      case ParameterizedDiagrams.Stack(topSubdiagram, bottomSubdiagram, polarity, properties, classes, id, gLabel) =>
        Stack(
          direct(topSubdiagram, direction),
          direct(bottomSubdiagram,
            polarity match { case PrePolarity.+ => direction; case _ => direction.reverse }),
          direction, polarity, properties, classes, id, gLabel)
