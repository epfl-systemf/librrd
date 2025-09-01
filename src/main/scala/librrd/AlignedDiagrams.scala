package librrd

import LayoutStylesheets.PropertyMap
import TipSpecification.*

object AlignedDiagrams:

  trait AlignedDiagramFields extends DirectedDiagrams.DirectedDiagramFields:
    val numRows: NumRows
    def toAlignedDiagram: AlignedDiagram
    def toParameterizedDiagram = toDirectedDiagram.toParameterizedDiagram

  sealed trait AlignedDiagram extends AlignedDiagramFields:
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

  case class Sequence(subdiagramsOneRow: Seq[AlignedDiagram],
                      subdiagramsMultiRow: Seq[AlignedDiagram],
                      direction: Direction,
                      properties: PropertyMap,
                      // if laid out in one row, ignored
                      // if in multiple, Physical(p) respected, else ignored
                      tipSpecs: TipSpecifications,
                      classes: Set[String] = Set.empty,
                      id: Option[String] = None) extends AlignedDiagram:

    val justifyContent = properties.get(LayoutStylesheets.JustifyContent)
    val sidemosts = SidedProperty(subdiagramsOneRow.head, subdiagramsOneRow.last)
    val numRows = NumRows.forEach(s =>
      if justifyContent.flush(s, direction) then sidemosts(s).numRows(s) else 1)

    Side.values.foreach(s =>
      assert(tipSpecs(s) match
          case Logical(r) => 1 == r
          case Physical(p) => 0 <= p && p <= 1
          case _ => true,
        s"invalid tip specification ${tipSpecs(s)} on side $s"))

    def toDirectedDiagram =
      DirectedDiagrams.Sequence(subdiagramsOneRow.map(_.toDirectedDiagram),
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

    val innerNumRows = NumRows.forEach(s => bottomSubdiagram.numRows(s)
      + (polarity match { case Polarity.+ => topSubdiagram.numRows(s); case _ => 1 }))
    val numRows = NumRows.forEach(s =>
      tipSpecs(s) match
        case Vertical => innerNumRows(s)
        case _ => 1)

    Side.values.foreach(s =>
      assert(tipSpecs(s) match
          case Logical(r) => 1 <= r && r <= innerNumRows(s)
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

    def singletonWithSpaces(ads: List[AlignedDiagram], direction: Direction, properties: PropertyMap) =
      if ads.length == 1 then ads(0)
      else Sequence(ads, ads, direction, properties,
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
          val tipSpecs = TipSpecifications.forEach(s => connectability(s) match
            case Neither => properties.get(LayoutStylesheets.AlignSelf)(s) match
              case AlignItemsPolicy.Top => Physical(0)
              case AlignItemsPolicy.Center => Physical(0.5)
              case AlignItemsPolicy.Bottom => Physical(1)
              case AlignItemsPolicy.Baseline => /* doesn't matter */ Logical(1)
            case _ => Vertical)

          val (alignedSubdiagramsOne, alignedSubdiagramsMulti) = subdiagrams.length match
            case 0 =>
              val spaces = Side.values.toList.flatMap(s =>
                if connectability(s) != Neither then Some(Space(direction)) else None)
              (spaces, spaces)

            case 1 =>
              val inner = rec(subdiagrams(0), SidedProperty.forEach(s =>
                if justifyContent.flush(s, direction) then connectability(s) else Neither))
              (inner, inner)

            case _ =>
              val (first, mids, last) = splitEnds(subdiagrams)
              val ends = SidedProperty(first, last)

              val spacesCondOne = SidedProperty.forEach(s => connectability(s) != Neither)
              val extraP = SidedProperty.apply.tupled(direction.swap((0, 1)))
              val extraConn = SidedProperty.apply[Connectable].tupled(direction.swap((Down,  Up)))
              val connectabilityMulti = SidedProperty.forEach(s => tipSpecs(s) match
                case Physical(p) if p != extraP(s) => extraConn(s)
                case _ => connectability(s))
              val spacesCondMulti = SidedProperty.forEach(s =>
                connectability(s) != Neither
                || (tipSpecs(s) match { case Physical(p) => p != extraP(s); case _ => false }))

              val alignedEndsOneMulti = SidedProperty.forEach{ s =>
                val aligneds =
                  if justifyContent.flush(s, direction) then
                    List(connectability(s), connectabilityMulti(s)).map(conn =>
                      (rec(ends(s), SidedProperty(Neither, Neither).update(s, conn))))
                  else
                    List(spacesCondOne(s), spacesCondMulti(s)).map(cond =>
                      maybeSurroundSpaces(
                        assertSingletonList(rec(ends(s), SidedProperty(Neither, Neither))),
                        SidedProperty(false, false).update(s, cond)))
                (aligneds(0), aligneds(1))
              }

              val alignedMids = mids.map(d => assertSingletonList(rec(d, SidedProperty(Neither, Neither))))
              (alignedEndsOneMulti(Side.Left)(0) ++ alignedMids ++ alignedEndsOneMulti(Side.Right)(0),
               alignedEndsOneMulti(Side.Left)(1) ++ alignedMids ++ alignedEndsOneMulti(Side.Right)(1))

          List(Sequence(alignedSubdiagramsOne, alignedSubdiagramsMulti,
                        direction, properties, tipSpecs, classes, id))


        case DirectedDiagrams.Stack(topSubdiagram, bottomSubdiagram,
                                    direction, polarity, properties, classes, id) =>
          val alignedTop = singletonWithSpaces(rec(topSubdiagram, SidedProperty(Down, Down)),
            topSubdiagram.direction, properties)
          val alignedBottom = singletonWithSpaces(rec(bottomSubdiagram, SidedProperty(Up, Up)),
            bottomSubdiagram.direction, properties)

          val tipSpecs = TipSpecifications.forEach(s => (connectability(s), polarity) match
            case (Neither, _) | (_, Polarity.-) => properties.get(LayoutStylesheets.AlignSelf)(s) match
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
                       case Polarity.- => 1) + alignedBottom.numRows(s) + 1)/2)
            case _ => Vertical)

          maybeSurroundSpaces(
            BlockVerticalConcatenation(
              alignedTop, alignedBottom, direction, polarity, properties, tipSpecs, classes, id),
            SidedProperty.forEach(s => connectability(s) != Neither && tipSpecs(s) != Vertical))


    assertSingletonList(rec(diagram, SidedProperty(Neither, Neither)))
