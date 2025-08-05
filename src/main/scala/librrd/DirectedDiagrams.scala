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
                     id: Option[String] = None)
      extends DirectedDiagram:
    def toParameterizedDiagram =
      ParameterizedDiagrams.Station(label, isTerminal, properties, classes, id)

  case class Sequence(subdiagrams: Seq[DirectedDiagram],
                      direction: Direction,
                      properties: LayoutStylesheets.PropertyMap,
                      classes: Set[String] = Set.empty,
                      id: Option[String] = None)
      extends DirectedDiagram:
    def toParameterizedDiagram =
      ParameterizedDiagrams.Sequence(subdiagrams.map(_.toParameterizedDiagram),
                                     properties, classes, id)

  case class Stack(topSubdiagram: DirectedDiagram,
                   bottomSubdiagram: DirectedDiagram,
                   direction: Direction,
                   polarity: Polarity,
                   properties: LayoutStylesheets.PropertyMap,
                   classes: Set[String] = Set.empty,
                   id: Option[String] = None)
      extends DirectedDiagram:
    def toParameterizedDiagram =
      ParameterizedDiagrams.Stack(topSubdiagram.toParameterizedDiagram,
                                  bottomSubdiagram.toParameterizedDiagram,
                                  polarity, properties, classes, id)


  def direct(diagram: ParameterizedDiagrams.ParameterizedDiagram, direction: Direction = LTR)
      : DirectedDiagram =
    diagram match
      case ParameterizedDiagrams.Station(label, isTerminal, properties, classes, id) =>
        Station(label, isTerminal, direction, properties, classes, id)
      case ParameterizedDiagrams.Sequence(subdiagrams, properties, classes, id) =>
        Sequence(
          direction.reverse(subdiagrams).map(direct(_, direction)),
          direction, properties.resolveStartEnd(direction), classes, id)
      case ParameterizedDiagrams.Stack(topSubdiagram, bottomSubdiagram, polarity, properties, classes, id) =>
        Stack(
          direct(topSubdiagram, direction),
          direct(bottomSubdiagram,
            polarity match { case Polarity.- => direction.reverse; case _ => direction }),
          direction, polarity, properties.resolveStartEnd(direction), classes, id)
