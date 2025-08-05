package librrd

import LayoutStylesheets.PropertyMap

object AlignedDiagrams:

  type NumRows = SidedProperty[Int]
  object NumRows extends SidedPropertyCompanion[Int]

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
                      // if laid out in one row, ignored
                      // if in multiple, Physical(p) respected, else ignored
                      tipSpecs: TipSpecifications,
                      classes: Set[String] = Set.empty,
                      id: Option[String] = None) extends AlignedDiagram:

    val justifyContent = properties.get(LayoutStylesheets.JustifyContent)
    val sidemosts = SidedProperty[AlignedDiagram].apply.tupled(
      direction.swap((subdiagrams.head, subdiagrams.last)))
    val numRows = NumRows.forEach(s =>
      if justifyContent.flush(s, direction) then sidemosts(s).numRows(s) else 1)

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

    val numRows = NumRows.forEach(s =>
      tipSpecs(s) match
        case TipSpecification.Vertical => bottomSubdiagram.numRows(s)
          + (polarity match { case Polarity.+ => topSubdiagram.numRows(s); case _ => 1 })
        case _ => 1)

    def toDirectedDiagram =
      DirectedDiagrams.Stack(topSubdiagram.toDirectedDiagram, bottomSubdiagram.toDirectedDiagram,
                             direction, polarity, properties, classes, id)


  def align(diagram: DirectedDiagrams.DirectedDiagram): AlignedDiagram =
    enum Connectable { case Up, Down, Neither }
    import Connectable.*
    def maybeSurroundSpaces(ad: AlignedDiagram, conditions: SidedProperty[Boolean])
        : AlignedDiagram =
      val withSpaces = ((if conditions.left then List(Space(ad.direction)) else List()) :+ ad)
        ++ (if conditions.right then List(Space(ad.direction)) else List())
      if withSpaces.length == 1 then withSpaces(0)
      else Sequence(withSpaces, ad.direction, ad.properties, ???, Set.empty, None)

    def rec(diagram: DirectedDiagrams.DirectedDiagram, connectability: SidedProperty[Connectable])
        : AlignedDiagram =
      diagram match
        case DirectedDiagrams.Station(label, isTerminal, direction, properties, classes, id) =>
          maybeSurroundSpaces(
            Station(label, isTerminal, direction, properties, classes, id),
            connectability.map(_ != Neither))

        case DirectedDiagrams.Sequence(subs, direction, properties, classes, id) =>
          val subdiagrams = subs.toVector
          val alignedSubdiagrams =
            if subdiagrams.length >= 2 then
              val (firsts, rest) = subdiagrams.splitAt(1)
              val (mids, lasts) = rest.splitAt(rest.length - 1)
              val (first, last) = (firsts(0), lasts(0))
              // TODO: only if flush!
              rec(first, SidedProperty(connectability.left, Neither))
              +: mids.map(rec(_, SidedProperty(Neither, Neither)))
              :+ rec(last, SidedProperty(Neither, connectability.right))
            else subdiagrams.map(rec(_, connectability))
          Sequence(alignedSubdiagrams, direction, properties, ???, classes, id)

        case DirectedDiagrams.Stack(topSubdiagram, bottomSubdiagram,
                                    direction, polarity, properties, classes, id) =>
          val alignedTop = rec(topSubdiagram, SidedProperty(Down, Down))
          val alignedBottom = rec(bottomSubdiagram, SidedProperty(Up, Up))

          val tipSpecs = TipSpecifications.forEach(s => (connectability(s), polarity) match
            case (Neither, _) | (_, Polarity.-) => properties.get(LayoutStylesheets.AlignItems) match
              case AlignItemsPolicy.Top => TipSpecification.Physical(0)
              case AlignItemsPolicy.Center => TipSpecification.Physical(0.5)
              case AlignItemsPolicy.Bottom => TipSpecification.Physical(1)
              case AlignItemsPolicy.Baseline =>
                topSubdiagram match
                  case DirectedDiagrams.Sequence(subs, _, _, _, _)
                    if subs.isEmpty && alignedBottom.numRows(s) == 1 => TipSpecification.Logical(2)
                  case _ => TipSpecification.Logical((
                    (polarity match
                       case Polarity.+ => alignedTop.numRows(s)
                       case Polarity.- => 1) + alignedBottom.numRows(s))/2 + 1)
            case _ => TipSpecification.Vertical)

          maybeSurroundSpaces(
            BlockVerticalConcatenation(
              alignedTop, alignedBottom, direction, polarity, properties, tipSpecs, classes, id),
            tipSpecs.map(_ != TipSpecification.Vertical))

    rec(diagram, SidedProperty(Neither,Neither))

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
