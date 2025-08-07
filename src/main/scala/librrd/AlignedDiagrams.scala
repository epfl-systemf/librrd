package librrd

import LayoutStylesheets.PropertyMap
import TipSpecification.*

object AlignedDiagrams:

  type NumRows = SidedProperty[Int]
  object NumRows extends SidedPropertyCompanion[Int]

  trait AlignedDiagramFields extends DirectedDiagrams.DirectedDiagramFields:
    def toAlignedDiagram: AlignedDiagram
    def toParameterizedDiagram = toDirectedDiagram.toParameterizedDiagram

  sealed trait AlignedDiagram extends AlignedDiagramFields:
    val numRows: NumRows
    def toAlignedDiagram = this

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

    Side.values.foreach(s =>
      assert(tipSpecs(s) match
          case Logical(r) => 1 == r
          case Physical(p) => 0 <= p && p <= 1
          case _ => true,
        s"invalid tip specification ${tipSpecs(s)} on side $s"))

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
        case Vertical => bottomSubdiagram.numRows(s)
          + (polarity match { case Polarity.+ => topSubdiagram.numRows(s); case _ => 1 })
        case _ => 1)

    Side.values.foreach(s =>
      assert(tipSpecs(s) match
          case Logical(r) => 1 <= r && r <= numRows(s)
          case Physical(p) => 0 <= p && p <= 1
          case _ => true,
        s"invalid tip specification ${tipSpecs(s)} on side $s with ${numRows(s)} rows"))

    def toDirectedDiagram =
      DirectedDiagrams.Stack(topSubdiagram.toDirectedDiagram, bottomSubdiagram.toDirectedDiagram,
                             direction, polarity, properties, classes, id)


  def align(diagram: DirectedDiagrams.DirectedDiagram): AlignedDiagram =
    enum Connectable { case Up, Down, Neither }
    import Connectable.*

    def maybeSurroundSpaces(ad: AlignedDiagram, conditions: SidedProperty[Boolean])
        : List[AlignedDiagram] =
      val maybeSpaces = conditions.map(c => if c then List(Space(ad.direction)) else List())
      maybeSpaces.left ++ (ad +: maybeSpaces.right)

    def singletonWithSpaces(ads: List[AlignedDiagram], properties: PropertyMap) =
      if ads.length == 1 then ads(0)
      else Sequence(ads, ads(0).direction, properties,
        TipSpecifications(Vertical, Vertical), Set.empty, None)

    def rec(diagram: DirectedDiagrams.DirectedDiagram, connectability: SidedProperty[Connectable])
        : List[AlignedDiagram] =
      diagram match
        case DirectedDiagrams.Station(label, isTerminal, direction, properties, classes, id) =>
          maybeSurroundSpaces(
            Station(label, isTerminal, direction, properties, classes, id),
            connectability.map(_ != Neither))


        case DirectedDiagrams.Sequence(subs, direction, properties, classes, id) =>
          val justifyContent = properties.get(LayoutStylesheets.JustifyContent)
          val subdiagrams = subs.toVector
          val alignedSubdiagrams = subdiagrams.length match
            case 0 => Side.values.toList.flatMap(s =>
              if connectability(s) != Neither then Some(Space(direction)) else None)

            case 1 => List(assertSingletonList(rec(subdiagrams(0), connectability)))

            case _ =>
              val (firsts, rest) = subdiagrams.splitAt(1)
              val (mids, lasts) = rest.splitAt(rest.length - 1)
              val (first, last) = (firsts(0), lasts(0))

              val alignedFirst =
                if justifyContent.flush(Side.Left, direction) then
                  rec(first, SidedProperty(connectability.left, Neither))
                else
                  maybeSurroundSpaces(
                    assertSingletonList(rec(first, SidedProperty(Neither, Neither))),
                    SidedProperty(connectability.left != Neither, false))

              val alignedLast =
                if justifyContent.flush(Side.Right, direction) then
                  rec(last, SidedProperty(Neither, connectability.right))
                else
                  maybeSurroundSpaces(
                    assertSingletonList(rec(last, SidedProperty(Neither, Neither))),
                    SidedProperty(false, connectability.right != Neither))

              alignedFirst
              ++ mids.map(d => assertSingletonList(rec(d, SidedProperty(Neither, Neither))))
              ++ alignedLast

          val tipSpecs = connectability.map(_ match
            case Neither => properties.get(LayoutStylesheets.AlignItems) match
              case AlignItemsPolicy.Top => Physical(0)
              case AlignItemsPolicy.Center => Physical(0.5)
              case AlignItemsPolicy.Bottom => Physical(1)
              case AlignItemsPolicy.Baseline => /* doesn't matter */ Logical(1)
            case _ => Vertical)
          List(Sequence(alignedSubdiagrams, direction, properties, tipSpecs, classes, id))


        case DirectedDiagrams.Stack(topSubdiagram, bottomSubdiagram,
                                    direction, polarity, properties, classes, id) =>
          val alignedTop =
            singletonWithSpaces(rec(topSubdiagram, SidedProperty(Down, Down)), properties)
          val alignedBottom =
            singletonWithSpaces(rec(bottomSubdiagram, SidedProperty(Up, Up)), properties)

          val tipSpecs = TipSpecifications.forEach(s => (connectability(s), polarity) match
            case (Neither, _) | (_, Polarity.-) => properties.get(LayoutStylesheets.AlignItems) match
              case AlignItemsPolicy.Top => Physical(0)
              case AlignItemsPolicy.Center => Physical(0.5)
              case AlignItemsPolicy.Bottom => Physical(1)
              case AlignItemsPolicy.Baseline =>
                topSubdiagram match
                  case DirectedDiagrams.Sequence(subs, _, _, _, _)
                    if subs.isEmpty && alignedBottom.numRows(s) == 1 => Logical(2)
                  case _ => Logical((
                    (polarity match
                       case Polarity.+ => alignedTop.numRows(s)
                       case Polarity.- => 1) + alignedBottom.numRows(s))/2 + 1)
            case _ => Vertical)

          maybeSurroundSpaces(
            BlockVerticalConcatenation(
              alignedTop, alignedBottom, direction, polarity, properties, tipSpecs, classes, id),
            tipSpecs.map(_ != Vertical))


    assertSingletonList(rec(diagram, SidedProperty(Neither, Neither)))
