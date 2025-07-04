package librrd

import LayoutStylesheets.*

enum ParameterizedDiagram:
  val id: Option[String]
  val classes: Set[String]

  case Station(label: String,
               isTerminal: Boolean,
               classes: Set[String] = Set.empty,
               id: Option[String] = None)

  case Sequence(subdiagrams: Seq[Diagram],
                classes: Set[String] = Set.empty,
                id: Option[String] = None,
                alignItems: AlignItemsPolicy,
                alignSelf: AlignItemsPolicy,
                justifyContent: JustifyContentPolicy,
                flexAbsorb: Double,
                gap: Double)

  case Stack(topSubdiagram: Diagram,
             bottomSubdiagram: Diagram,
             polarity: Polarity,
             classes: Set[String] = Set.empty,
             id: Option[String] = None,
             alignItems: AlignItemsPolicy,
             alignSelf: AlignItemsPolicy,
             justifyContent: JustifyContentPolicy,
             flexAbsorb: Double,
             gap: Double)


def parameterize(diagram: Diagram, stylesheet: Stylesheet): ParameterizedDiagram =
  def rec(diagram: Diagram, parents: Seq[TagInfo]) =
    diagram match
      case Diagram.TerminalToken(label, classes, id) =>

  rec(diagram, Seq.empty)
  ???
