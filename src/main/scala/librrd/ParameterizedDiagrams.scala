package librrd

import LayoutStylesheets.*

object ParameterizedDiagrams:

  trait ParameterizedDiagramFields extends Diagrams.DiagramFields:
    val properties: PropertyMap
    def toParameterizedDiagram: ParameterizedDiagram

  sealed trait ParameterizedDiagram extends ParameterizedDiagramFields:
    def toParameterizedDiagram = this


  case class Station(label: String,
                     isTerminal: Boolean,
                     properties: PropertyMap,
                     classes: Set[String] = Set.empty,
                     id: Option[String] = None)
      extends ParameterizedDiagram:
    def toDiagram =
      if isTerminal
      then Diagrams.TerminalToken(label, classes, id)
      else Diagrams.NonterminalToken(label, classes, id)

  case class Sequence(subdiagrams: Seq[ParameterizedDiagram],
                      properties: PropertyMap,
                      classes: Set[String] = Set.empty,
                      id: Option[String] = None)
      extends ParameterizedDiagram:
    def toDiagram = Diagrams.Sequence(subdiagrams.map(_.toDiagram), classes, id)

  case class Stack(topSubdiagram: ParameterizedDiagram,
                   bottomSubdiagram: ParameterizedDiagram,
                   polarity: Polarity,
                   properties: PropertyMap,
                   classes: Set[String] = Set.empty,
                   id: Option[String] = None)
      extends ParameterizedDiagram:
    def toDiagram =
      Diagrams.Stack(topSubdiagram.toDiagram, bottomSubdiagram.toDiagram,
                     polarity, classes, id)


  def parameterize(diagram: Diagrams.Diagram, stylesheet: Stylesheet): ParameterizedDiagram =
    def rec(diagram: Diagrams.Diagram, parents: List[TagInfo]): ParameterizedDiagram =
      diagram match

        case Diagrams.TerminalToken(label, classes, id) =>
          val info = TagInfo(Tag.TerminalToken, id, classes.toSet)
          Station(
            label, true,
            stylesheet.mostSpecificProperties(info, parents),
            info.classes, id)
        case Diagrams.NonterminalToken(label, classes, id) =>
          val info = TagInfo(Tag.NonterminalToken, id, classes.toSet)
          Station(
            label, false,
            stylesheet.mostSpecificProperties(info, parents),
            info.classes, id)

        case Diagrams.Sequence(subdiagrams, classes, id) =>
          val info = TagInfo(Tag.Sequence, id, classes.toSet)
          Sequence(
            subdiagrams.map(rec(_, info :: parents)),
            stylesheet.mostSpecificProperties(info, parents),
            info.classes, id)

        case Diagrams.Stack(topSubdiagram, bottomSubdiagram, polarity, classes, id) =>
          val info = TagInfo(Tag.Stack, id, classes.toSet)
          Stack(
            rec(topSubdiagram, info :: parents),
            rec(bottomSubdiagram, info :: parents),
            polarity,
            stylesheet.mostSpecificProperties(info, parents),
            info.classes, id)

    rec(diagram, List())
