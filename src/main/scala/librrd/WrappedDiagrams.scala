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
                     font: FontInfo,
                     classes: Set[String] = Set.empty,
                     id: Option[String] = None) extends LocallyWrappedDiagram, GlobalWrap:
    val minContent = backend.measure(label, font)._1 + 4*backend.Station.paddingX
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

    val extraWidths = SidedProperty.forEach(s =>
      if tipSpecs(s) == TipSpecification.Vertical then 0 else 3*unitWidth)
    val extraWidth = extraWidths.left + extraWidths.right
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

    val markerFont = LayoutStylesheets.Font.default
    val markerWidth =
      backend.measure(properties.get(LayoutStylesheets.ContinuationMarker), markerFont)._1
      + 2*backend.InlineVerticalConcatenation.markerPadding

    val extraP = SidedProperty.apply.tupled(direction.swap((0, 1)))
    val extraWidths = SidedProperty.forEach(s => tipSpecs(s) match
        case TipSpecification.Physical(p) if p != extraP(s) => 3*unitWidth
        case _ => 0)
    val extraWidth = extraWidths.left + extraWidths.right
    val (first, mids, last) = splitEnds(subdiagrams)
    val minContent = (List(first, last).map(_.minContent + markerWidth)
                      ++ mids.map(_.minContent + 2*markerWidth)).max + extraWidth
    val maxContent = (List(first, last).map(_.maxContent + markerWidth)
                      ++ mids.map(_.maxContent + 2*markerWidth)).max + extraWidth


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
      val (maybeSpaces, withoutSpaces) = trimSides(subdiagrams, { case sp: Space => sp })
      allPartitions(withoutSpaces.toList).map[SequenceWrap[LocallyWrappedDiagram]](partition =>
        if partition.length == 1
        then HorizontalConcatenation(
          maybeSpaces.left.toList ++ partition(0) ++ maybeSpaces.right.toList,
          direction, properties, numRows, classes, id)
        else
          val extraP = SidedProperty.apply.tupled(direction.swap((0, 1)))
          val extraSpaces = SidedProperty.forEach(s => tipSpecs(s) match
            case TipSpecification.Physical(p) if p != extraP(s) =>
              Some(Space(direction, NumRows(1, 1)))
            case _ => maybeSpaces(s))
          val (first, mids, last) = splitEnds(partition)
          val withSpaces = (extraSpaces.left.toList ++ first)
            +: mids :+ (last ++ extraSpaces.right.toList)
          InlineVerticalConcatenation(
            direction.reverse(withSpaces.map(rowSubs =>
              HorizontalConcatenation(rowSubs, direction, properties, NumRows(1, 1), classes, None))),
            direction, properties, tipSpecs,
            NumRows.apply.tupled(direction.swap((numRows.left, numRows.right))), classes, id))

    val minContentOption = options.minBy(_.minContent)
    val minContent: Double = minContentOption.minContent
    val maxContent: Double = options.last.maxContent

    def toAlignedDiagram =
      AlignedDiagrams.Sequence(subdiagrams.map(_.toAlignedDiagram),
        direction, properties, tipSpecs, classes, id)

    def bestUnder(width: Double): SequenceWrap[LocallyWrappedDiagram] =
      options
        .filter(_.minContent <= width)
        // make wrapping magic happen here
        .minByOption(w => (w.maxContent - width).abs)
        .getOrElse(minContentOption)


  def wrapLocally(diagram: AlignedDiagrams.AlignedDiagram): LocallyWrappedDiagram =
    diagram match
      case AlignedDiagrams.Station(label, isTerminal, direction, properties, classes, id) =>
        Station(label, isTerminal, direction, properties, diagram.numRows,
                properties.get(LayoutStylesheets.Font), classes, id)
      case AlignedDiagrams.Space(direction) =>
        Space(direction, diagram.numRows)
      case AlignedDiagrams.Sequence(subdiagrams, direction, properties, tipSpecs, classes, id) =>
        LocallyWrappedSequence(subdiagrams.map(wrapLocally),
          direction, properties, tipSpecs, diagram.numRows, classes, id)
      case AlignedDiagrams.BlockVerticalConcatenation(topSubdiagram, bottomSubdiagram,
          direction, polarity, properties, tipSpecs, classes, id) =>
        BlockVerticalConcatenation(wrapLocally(topSubdiagram), wrapLocally(bottomSubdiagram),
          direction, polarity, properties, tipSpecs, diagram.numRows, classes, id)
