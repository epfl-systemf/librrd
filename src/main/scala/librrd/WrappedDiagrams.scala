package librrd

import LayoutStylesheets.PropertyMap

class WrappedDiagrams[T](val backend: Layouts[T]):
  val unitWidth = backend.Layout.unitWidth

  trait WrappedDiagramFields extends AlignedDiagrams.AlignedDiagramFields:
    def toWrappedDiagram: WrappedDiagram
    def toDirectedDiagram = toAlignedDiagram.toDirectedDiagram

  trait ContentWidthFields:
    val minContent: Double
    val maxContent: Double

  sealed trait WrappedDiagram extends WrappedDiagramFields, ContentWidthFields:
    def toWrappedDiagram = this

  sealed trait LocallyWrappedDiagram extends WrappedDiagram
  sealed trait GlobalWrap extends ContentWidthFields

  case class Station(label: String,
                     isTerminal: Boolean,
                     direction: Direction,
                     properties: PropertyMap,
                     numRows: NumRows,
                     classes: Set[String] = Set.empty,
                     id: Option[String] = None) extends LocallyWrappedDiagram, GlobalWrap:
    val minContent = backend.measure(label)._1 + 4*backend.Station.paddingX
    val maxContent = minContent
    def toAlignedDiagram =
      AlignedDiagrams.Station(label, isTerminal, direction, properties, classes, id)

  case class Space(direction: Direction, numRows: NumRows)
      extends LocallyWrappedDiagram, GlobalWrap:
    val properties = PropertyMap(Seq())
    val classes = Set()
    val id = None
    val minContent = 2*unitWidth
    val maxContent = minContent
    def toAlignedDiagram = AlignedDiagrams.Space(direction)


  case class BlockVerticalConcatenation(
      topSubdiagram: WrappedDiagram,
      bottomSubdiagram: WrappedDiagram,
      direction: Direction,
      polarity: Polarity,
      properties: PropertyMap,
      tipSpecs: TipSpecifications,
      numRows: NumRows,
      classes: Set[String] = Set.empty,
      id: Option[String] = None) extends LocallyWrappedDiagram, GlobalWrap:

    val extraWidth: Double =
      + Side.values.map(s => (tipSpecs(s), polarity) match
          case (TipSpecification.Vertical, Polarity.+) => 0
          case (TipSpecification.Vertical
              | TipSpecification.Logical(1)
              | TipSpecification.Physical(0), Polarity.-) => 2 * unitWidth
          case _ => 3 * unitWidth).sum
    val minContent = Math.max(topSubdiagram.minContent, bottomSubdiagram.minContent) + extraWidth
    val maxContent = Math.max(topSubdiagram.maxContent, bottomSubdiagram.maxContent) + extraWidth

    def toAlignedDiagram = AlignedDiagrams.BlockVerticalConcatenation(
      topSubdiagram.toAlignedDiagram, bottomSubdiagram.toAlignedDiagram,
      direction, polarity, properties, tipSpecs, classes, id)


  sealed trait SequenceWrap[+D <: ContentWidthFields] extends ContentWidthFields

  case class HorizontalConcatenation[+D <: ContentWidthFields](
      subdiagrams: Seq[D | InlineVerticalConcatenation[D]],
      direction: Direction,
      properties: PropertyMap,
      numRows: NumRows,
      classes: Set[String] = Set.empty,
      id: Option[String] = None) extends SequenceWrap[D]:
    val minGap = (if subdiagrams.isEmpty then 0 else (subdiagrams.length - 1) * MIN_GAP)
    val minContent = subdiagrams.map(_.minContent).sum + minGap
    val maxContent = subdiagrams.map(_.maxContent).sum + minGap


  case class InlineVerticalConcatenation[+D <: ContentWidthFields](
      subdiagrams: Seq[D | HorizontalConcatenation[D]],
      direction: Direction,
      properties: PropertyMap,
      tipSpecs: TipSpecifications,
      numRows: NumRows,
      classes: Set[String] = Set.empty,
      id: Option[String] = None) extends SequenceWrap[D]:

    assert(subdiagrams.length >= 2,
      "inline vertical concatenation must have at least 2 subdiagrams")

    val markerWidth = backend.measure(properties.get(LayoutStylesheets.ContinuationMarker))._1

    val extraWidth: Double =
      markerWidth + 2*backend.InlineVerticalConcatenation.markerPadding
      + direction.reverse(Side.values).zip(List(0, 1)).map((s, p) => tipSpecs(s) match
          case TipSpecification.Physical(q) if q != p => 3 * unitWidth
          case _ => 0).sum
    val minContent = subdiagrams.map(_.minContent).max + extraWidth
    val maxContent = subdiagrams.map(_.maxContent).max + extraWidth


  case class GlobalSequenceWrap(sw: SequenceWrap[GlobalWrap]) extends GlobalWrap:
    val minContent = sw.minContent
    val maxContent = sw.maxContent


  sealed trait HasBestUnderWidth[T]:
    def bestUnder(width: Double): T


  case class GloballyWrappedDiagram(
      direction: Direction,
      properties: PropertyMap,
      numRows: NumRows, // TODO
      options: Iterable[GlobalWrap]) extends HasBestUnderWidth[GlobalWrap], WrappedDiagram:

    val classes: Set[String] = ???
    val id: Option[String] = ???

    val maxContent: Double = ???
    val minContent: Double = ???
    
    def toAlignedDiagram: librrd.AlignedDiagrams.AlignedDiagram = ???

    def bestUnder(width: Double): GlobalWrap = ???


  case class LocallyWrappedSequence(
        subdiagrams: Seq[LocallyWrappedDiagram],
        direction: Direction,
        properties: PropertyMap,
        tipSpecs: TipSpecifications,
        numRows: NumRows,
        classes: Set[String] = Set.empty,
        id: Option[String] = None)
      extends HasBestUnderWidth[SequenceWrap[LocallyWrappedDiagram]], LocallyWrappedDiagram:

    val options =
      allPartitions(subdiagrams.toList).map[SequenceWrap[LocallyWrappedDiagram]](partition =>
        if partition.length == 1
        then HorizontalConcatenation(partition(0), direction, properties, numRows, classes, id)
        else InlineVerticalConcatenation(
          direction.reverse(partition.map(rowSubs =>
            HorizontalConcatenation(rowSubs, direction, properties, NumRows(1, 1), classes, id))),
          direction, properties, tipSpecs,
          NumRows.apply.tupled(direction.swap((numRows.left, numRows.right))), classes, id))

    def allPartitions[T](l: List[T]): Vector[List[List[T]]] =
      l match
        case Nil => Vector(List(List()))
        case elem :: Nil => Vector(List(List(elem)))
        case head :: tail =>
          val restPartitions = allPartitions(tail)
          restPartitions.map(rp => List(head) +: rp)
          ++ restPartitions.map(rp => (head +: rp.head) +: rp.tail)

    val minContent: Double = options.map(_.minContent).min
    val maxContent: Double = options.last.maxContent

    def toAlignedDiagram =
      AlignedDiagrams.Sequence(subdiagrams.map(_.toAlignedDiagram),
        direction, properties, tipSpecs, classes, id)

    def bestUnder(width: Double): SequenceWrap[LocallyWrappedDiagram] =
      options
        .filter(_.minContent <= width)
        // make wrapping magic happen here
        .head


  def wrapLocally(diagram: AlignedDiagrams.AlignedDiagram): LocallyWrappedDiagram =
    diagram match
      case AlignedDiagrams.Station(label, isTerminal, direction, properties, classes, id) =>
        Station(label, isTerminal, direction, properties, diagram.numRows, classes, id)
      case AlignedDiagrams.Space(direction) =>
        Space(direction, diagram.numRows)
      case AlignedDiagrams.Sequence(subdiagrams, direction, properties, tipSpecs, classes, id) =>
        LocallyWrappedSequence(subdiagrams.map(wrapLocally),
          direction, properties, tipSpecs, diagram.numRows, classes, id)
      case AlignedDiagrams.BlockVerticalConcatenation(topSubdiagram, bottomSubdiagram,
          direction, polarity, properties, tipSpecs, classes, id) =>
        BlockVerticalConcatenation(wrapLocally(topSubdiagram), wrapLocally(bottomSubdiagram),
          direction, polarity, properties, tipSpecs, diagram.numRows, classes, id)
