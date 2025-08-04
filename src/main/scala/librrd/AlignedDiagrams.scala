package librrd

import LayoutStylesheets.PropertyMap

object AlignedDiagrams:

  case class NumRows(left: Int, right: Int)

  trait AlignedDiagramFields extends DirectedDiagrams.DirectedDiagramFields:
    val numRows: NumRows
    def toAlignedDiagram: AlignedDiagram

  sealed trait AlignedDiagram extends AlignedDiagramFields:
    def toAlignedDiagram = this
    def toDiagram = toDirectedDiagram.toDiagram
    def toParameterizedDiagram = toDirectedDiagram.toParameterizedDiagram

  case class Station(label: String,
                     isTerminal: Boolean,
                     direction: Direction,
                     properties: PropertyMap,
                     classes: Set[String] = Set.empty,
                     id: Option[String] = None) extends AlignedDiagram:
    val numRows = NumRows(1, 1)
    def toDirectedDiagram =
      DirectedDiagrams.Station(label, isTerminal, direction, properties, classes, id)

  case class Space(direction: Direction) extends AlignedDiagram:
    val properties = PropertyMap(Seq())
    val classes = Set()
    val id = None
    val numRows = NumRows(1, 1)
    def toDirectedDiagram =
      DirectedDiagrams.Sequence(Seq(), direction, properties)

  case class Sequence(subdiagrams: Seq[AlignedDiagram],
                      direction: Direction,
                      properties: PropertyMap,
                      tipSpecs: TipSpecifications,
                      classes: Set[String] = Set.empty,
                      id: Option[String] = None) extends AlignedDiagram:

    val justifyContent = properties.get(LayoutStylesheets.JustifyContent)
    val (leftmost, rightmost) = direction.swap((subdiagrams.head, subdiagrams.last))
    val numRows = NumRows(
      if justifyContent.flush(Side.Left, direction) then leftmost.numRows.left else 1,
      if justifyContent.flush(Side.Right, direction) then rightmost.numRows.right else 1)

    def toDirectedDiagram =
      DirectedDiagrams.Sequence(subdiagrams.map(_.toDirectedDiagram),
                                direction, properties, classes, id)


  case class BlockVerticalConcatenation(
      topSubdiagram: AlignedDiagram,
      bottomSubdiagram: AlignedDiagram,
      direction: Direction,
      polarity: Polarity,
      properties: PropertyMap,
      tipSpecs: TipSpecifications,
      classes: Set[String] = Set.empty,
      id: Option[String] = None) extends AlignedDiagram:

    val numRows = polarity match
      case Polarity.+ => NumRows(topSubdiagram.numRows.left + bottomSubdiagram.numRows.left,
                                 topSubdiagram.numRows.right + bottomSubdiagram.numRows.right)
      case Polarity.- => NumRows(1 + bottomSubdiagram.numRows.left,
                                 1 + bottomSubdiagram.numRows.right)

    def toDirectedDiagram =
      DirectedDiagrams.Stack(topSubdiagram.toDirectedDiagram, bottomSubdiagram.toDirectedDiagram,
                             direction, polarity, properties, classes, id)


  def align(diagram: DirectedDiagrams.DirectedDiagram): AlignedDiagram =
    enum Connectable { case Up, Down, Neither }
    import Connectable.*
    def maybeSurroundSpaces(ad: AlignedDiagram, leftCondition: Boolean, rightCondition: Boolean)
        : AlignedDiagram =
      val withSpaces = ((if leftCondition then List(Space(ad.direction)) else List()) :+ ad)
        ++ (if rightCondition then List(Space(ad.direction)) else List())
      if withSpaces.length == 1 then withSpaces(0)
      else Sequence(withSpaces, ad.direction, ad.properties, Set.empty, None)

    def rec(diagram: DirectedDiagrams.DirectedDiagram,
            leftC: Connectable, rightC: Connectable)
        : AlignedDiagram =
      diagram match
        case DirectedDiagrams.Station(label, isTerminal, direction, properties, classes, id) =>
          maybeSurroundSpaces(
            Station(label, isTerminal, direction, properties, classes, id),
            leftC != Neither, rightC != Neither)

        case DirectedDiagrams.Sequence(subs, direction, properties, classes, id) =>
          val subdiagrams = subs.toVector
          val alignedSubdiagrams =
            if subdiagrams.length >= 2 then
              val (firsts, rest) = subdiagrams.splitAt(1)
              val (mids, lasts) = rest.splitAt(rest.length - 1)
              val (first, last) = (firsts(0), lasts(0))
              // TODO: only if flush!
              rec(first, leftC, Neither)
              +: mids.map(rec(_, Neither, Neither))
              :+ rec(last, Neither, rightC)
            else subdiagrams.map(rec(_, leftC, rightC))
          Sequence(alignedSubdiagrams, direction, properties, classes, id)

        case DirectedDiagrams.Stack(topSubdiagram, bottomSubdiagram,
                                    direction, polarity, properties, classes, id) =>
          val alignedTop = rec(topSubdiagram, Down, Down)
          val alignedBottom = rec(bottomSubdiagram, Up, Up)

          def tipSpec(c: Connectable, topRows: Int, bottomRows: Int) = (c, polarity) match
            case (Neither, _) | (_, Polarity.-) => properties.get(LayoutStylesheets.AlignItems) match
              case AlignItemsPolicy.Top => TipSpecification.Physical(0)
              case AlignItemsPolicy.Center => TipSpecification.Physical(0.5)
              case AlignItemsPolicy.Bottom => TipSpecification.Physical(1)
              case AlignItemsPolicy.Baseline =>
                topSubdiagram match
                  case DirectedDiagrams.Sequence(subs, _, _, _, _)
                    if subs.isEmpty && bottomRows == 1 => TipSpecification.Logical(2)
                  case _ => TipSpecification.Logical((
                    (polarity match
                       case Polarity.+ => topRows
                       case Polarity.- => 1) + bottomRows)/2 + 1)
            case _ => TipSpecification.Vertical

          val tipSpecs = TipSpecifications(
            tipSpec(leftC, alignedTop.numRows.left, alignedBottom.numRows.left),
            tipSpec(rightC, alignedTop.numRows.right, alignedBottom.numRows.right))

          maybeSurroundSpaces(
            BlockVerticalConcatenation(
              alignedTop, alignedBottom, direction, polarity, properties, tipSpecs, classes, id),
            tipSpecs.left != TipSpecification.Vertical,
            tipSpecs.right != TipSpecification.Vertical)

    rec(diagram, Connectable.Neither, Connectable.Neither)

/*
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
*/
