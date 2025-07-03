package librrd

import RelativeSide.*
import TipSpecification.*

enum AlignItemsPolicy:
  case Top, Center, Bottom, Baseline
  def defaultTipSpecification(diagram: Diagram)(rs: RelativeSide): TipSpecification =
    import Diagram.*
    (this, diagram) match
      case (_, _: (TerminalToken | NonterminalToken)) => Logical(1)
      case (Top, _) => Physical(0)
      case (Bottom, _) => Physical(1)
      case (Baseline, stack: Stack)
        if stack.topSubdiagram == Sequence(Seq.empty)
          && stack.bottomSubdiagram.numRows(rs) == 1 => Logical(2)
      case (_, stack: Stack) => Logical(stack.numRows(rs) / 2 + 1)
      case (Baseline, sequence: Sequence) =>
        Baseline.defaultTipSpecification(rs match
          case Start => sequence.subdiagrams.head
          case End => sequence.subdiagrams.last)(rs)
      case (Center, _: Sequence) => Physical(0.5)


def align(diagram: Diagram, policy: AlignItemsPolicy, direction: Direction = Direction.LTR)
    : AlignedDiagram =
  def rec(diagram: Diagram, direction: Direction,
          tipSpec: PartialFunction[RelativeSide, TipSpecification])
      : List[AlignedDiagram] =
    val tipSpecDefault = tipSpec.orElse(policy.defaultTipSpecification(diagram))
    val absoluteTipSpecDefault = absoluteTipSpec(tipSpecDefault, direction)
    diagram match
      case token: (Diagram.TerminalToken | Diagram.NonterminalToken) =>
        def maybeSpace(rs: RelativeSide) =
          if tipSpecDefault(rs) == Vertical then List(AlignedDiagram.Space(direction)) else List()
        direction.reverse(maybeSpace(Start)
          ++ List(token match
            case Diagram.TerminalToken(label) => AlignedDiagram.Station(direction, true, label)
            case Diagram.NonterminalToken(label) => AlignedDiagram.Station(direction, false, label))
          ++ maybeSpace(End))

      case Diagram.Sequence(subdiagrams) =>
        val subAligned =
          rec(subdiagrams.head, direction, { case Start => tipSpecDefault(Start) })
          ++ subdiagrams.drop(1).dropRight(1).map(d =>
               assertSingletonList(rec(d, direction, PartialFunction.empty)))
          ++ rec(subdiagrams.head, direction, { case End => tipSpecDefault(End) })
        List(AlignedDiagram.Sequence(direction, absoluteTipSpecDefault, direction.reverse(subAligned)))

      case Diagram.Stack(topSubdiagram, bottomSubdiagram, polarity) =>
        List(AlignedDiagram.BlockVerticalConcatenation(
          direction,
          absoluteTipSpecDefault,
          polarity,
          assertSingletonList(rec(topSubdiagram, direction, _ => Vertical)),
          assertSingletonList(rec(bottomSubdiagram,
              if polarity == Polarity.+ then direction else direction.reverse,
              _ => Vertical))
        ))

  assertSingletonList(rec(diagram, direction, PartialFunction.empty))
