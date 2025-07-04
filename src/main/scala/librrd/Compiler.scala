package librrd

import TipSpecification.*

val MIN_GAP = 0.0

enum JustifyContentPolicy:
  case Start, Left, Right, End
  case SpaceBetween, SpaceAround, SpaceEvenly, Center

  import Direction.*

  def flush(s: Side, d: Direction): Boolean = (this, s, d) match
    case (SpaceBetween, _, _) => true
    case (Left, Side.Left, _) | (Right, Side.Right, _) => true
    case (Start, Side.Left, LTR) | (End, Side.Right, LTR) => true
    case (Start, Side.Right, RTL) | (End, Side.Left, RTL) => true
    case _ => false

  def distribute(space: Double, numItems: Int, direction: Direction): List[Double] =
    assert(space ~>= (numItems - 1)*MIN_GAP,
      "space to justify must be at least MIN_GAP*(N-1)")
    val extra = space - (numItems - 1)*MIN_GAP
    val distributed = (this, direction) match
      case (Left, _) | (Start, LTR) | (End, RTL) =>
        0.0 +: List.fill(numItems - 1)(MIN_GAP) :+ extra
      case (Right, _) | (Start, RTL) | (End, LTR) =>
        extra +: List.fill(numItems - 1)(MIN_GAP) :+ 0.0
      case (SpaceBetween, _) =>
        if numItems == 1 then List(space/2, space/2)
        else 0.0 +: List.fill(numItems - 1)(space/(numItems - 1)) :+ 0.0
      case (SpaceAround, _) =>
        val inner = Math.max(MIN_GAP, space/numItems)
        val outer = (space - inner*(numItems - 1))/2
        outer +: List.fill(numItems - 1)(inner) :+ outer
      case (SpaceEvenly, _) =>
        val inner = Math.max(MIN_GAP, space/(numItems + 1))
        val outer = (space - inner*(numItems - 1))/2
        outer +: List.fill(numItems - 1)(inner) :+ outer
      case (Center, _) =>
        extra/2 +: List.fill(numItems - 1)(MIN_GAP) :+ extra/2

    assert((distributed.length == numItems + 1)
        && (distributed.sum ~= space)
        && (distributed.drop(1).dropRight(1).forall(MIN_GAP ~<= _)),
      "justify-content implementation error")
    distributed


def numRows(diagram: Diagram, policy: JustifyContentPolicy,
            direction : Direction, side: Side): Int =
  diagram match
    case _: (Diagram.TerminalToken | Diagram.NonterminalToken) => 1
    case seq @ Diagram.Sequence(subdiagrams) =>
      if policy.flush(side, direction)
      then numRows(Diagram.sidemost(seq, side, direction), policy, direction, side)
      else 1
    case Diagram.Stack(topSubdiagram, bottomSubdiagram, polarity) =>
      polarity match
        case Polarity.+ =>
          numRows(topSubdiagram, policy, direction, side)
          + numRows(bottomSubdiagram, policy, direction, side)
        case Polarity.- => 1 + numRows(bottomSubdiagram, policy, direction.reverse, side)


enum AlignItemsPolicy:
  case Top, Center, Bottom, Baseline
  def defaultTipSpecification
      (diagram: Diagram, JCPolicy: JustifyContentPolicy, direction: Direction)
      (side: Side): TipSpecification =
    import Diagram.*
    (this, diagram) match
      case (_, _: (TerminalToken | NonterminalToken)) => Logical(1)
      case (Top, _) => Physical(0)
      case (Bottom, _) => Physical(1)
      case (Baseline, stack: Stack)
          if stack.topSubdiagram == Sequence(Seq.empty)
            && numRows(stack.bottomSubdiagram, JCPolicy, direction, side) == 1 =>
        Logical(2)
      case (_, stack: Stack) =>
       Logical(numRows(stack, JCPolicy, direction, side)/2 + 1)
      case (Baseline, sequence: Sequence) =>
        if JCPolicy.flush(side, direction)
        then Baseline.defaultTipSpecification
          (Diagram.sidemost(sequence, side, direction), JCPolicy, direction)(side)
        else Logical(1)
      case (Center, _: Sequence) => Physical(0.5)





def align(diagram: Diagram, alignItems: AlignItemsPolicy, justifyContent: JustifyContentPolicy,
          direction: Direction = Direction.LTR): AlignedDiagram =
  val defaultTipSpec = alignItems.defaultTipSpecification(diagram, justifyContent, direction)
  def rec(diagram: Diagram, direction: Direction,
          tipSpec: PartialFunction[Side, TipSpecification])
      : List[AlignedDiagram] =
    import Side.*
    val tipSpecWithDefault = tipSpec.orElse(defaultTipSpec(_))
    diagram match
      case token: (Diagram.TerminalToken | Diagram.NonterminalToken) =>
        def maybeSpace(s: Side) =
          if tipSpecWithDefault(s) == Vertical
          then List(AlignedDiagram.Space(direction))
          else List()
        maybeSpace(Left)
          ++ List(token match
            case Diagram.TerminalToken(label) => AlignedDiagram.Station(direction, true, label)
            case Diagram.NonterminalToken(label) => AlignedDiagram.Station(direction, false, label))
          ++ maybeSpace(Right)

      case Diagram.Sequence(subdiagrams) =>
        val maybeReversed = direction.reverse(subdiagrams)
        def maybeTipSpec(S: Side): PartialFunction[Side, TipSpecification] =
          { case S if justifyContent.flush(S, direction) => tipSpecWithDefault(S) }
        val subAligned =
          rec(maybeReversed.head, direction, maybeTipSpec(Left))
          ++ maybeReversed.drop(1).dropRight(1).map(d =>
               assertSingletonList(rec(d, direction, PartialFunction.empty)))
          ++ rec(maybeReversed.last, direction, maybeTipSpec(Right))
        List(AlignedDiagram.Sequence(direction, tipSpecWithDefault, subAligned))

      case Diagram.Stack(topSubdiagram, bottomSubdiagram, polarity) =>
        List(AlignedDiagram.BlockVerticalConcatenation(
          direction,
          tipSpecWithDefault,
          polarity,
          assertSingletonList(rec(topSubdiagram, direction, _ => Vertical)),
          assertSingletonList(rec(bottomSubdiagram,
              if polarity == Polarity.+ then direction else direction.reverse,
              PartialFunction.empty))
        ))

  assertSingletonList(rec(diagram, direction, PartialFunction.empty))


// def wrapAndJustify(aligned: AlignedDiagram):
