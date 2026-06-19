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
                     id: Option[String] = None,
                     groupLabel: Option[String] = None)
      extends ParameterizedDiagram:
    def toDiagram =
      if isTerminal
      then Diagrams.TerminalToken(label, classes, id, groupLabel)
      else Diagrams.NonterminalToken(label, classes, id, groupLabel)

  case class Sequence(subdiagrams: Seq[ParameterizedDiagram],
                      properties: PropertyMap,
                      classes: Set[String] = Set.empty,
                      id: Option[String] = None,
                      groupLabel: Option[String] = None)
      extends ParameterizedDiagram:
    def toDiagram = Diagrams.Sequence(subdiagrams.map(_.toDiagram), classes, id, groupLabel)

  case class Stack(topSubdiagram: ParameterizedDiagram,
                   bottomSubdiagram: ParameterizedDiagram,
                   polarity: PrePolarity,
                   properties: PropertyMap,
                   classes: Set[String] = Set.empty,
                   id: Option[String] = None,
                   groupLabel: Option[String] = None)
      extends ParameterizedDiagram:
    def toDiagram =
      Diagrams.Stack(topSubdiagram.toDiagram, bottomSubdiagram.toDiagram,
                     polarity, classes, id, groupLabel)


  def parameterize(diagram: Diagrams.Diagram, stylesheet: Stylesheet): ParameterizedDiagram =
    def rec(diagram: Diagrams.Diagram, parents: List[TagInfo], inheritable: PropertyMap)
        : ParameterizedDiagram =
      diagram match

        case Diagrams.TerminalToken(label, classes, id, gLabel) =>
          val info = TagInfo(Tag.TerminalToken, id, classes.toSet)
          Station(
            label, true,
            stylesheet.mostSpecificProperties(info, parents, inheritable),
            info.classes, id, gLabel)
        case Diagrams.NonterminalToken(label, classes, id, gLabel) =>
          val info = TagInfo(Tag.NonterminalToken, id, classes.toSet)
          Station(
            label, false,
            stylesheet.mostSpecificProperties(info, parents, inheritable),
            info.classes, id, gLabel)

        case Diagrams.Sequence(subdiagrams, classes, id, gLabel) =>
          val info = TagInfo(Tag.Sequence, id, classes.toSet)
          val properties = stylesheet.mostSpecificProperties(info, parents, inheritable)
          Sequence(
            subdiagrams.map(rec(_, info :: parents, properties.filterInheritable)),
            properties, info.classes, id, gLabel)

        case Diagrams.Stack(topSubdiagram, bottomSubdiagram, polarity, classes, id, gLabel) =>
          val info = TagInfo(Tag.Stack, id, classes.toSet)
          val properties = stylesheet.mostSpecificProperties(info, parents, inheritable)
          val subInheritable = properties.filterInheritable
          Stack(
            rec(topSubdiagram, info :: parents, subInheritable),
            rec(bottomSubdiagram, info :: parents, subInheritable),
            polarity, properties, info.classes, id, gLabel)

    rec(diagram, List(), defaultProperties)
