package librrd

import LayoutStylesheets.PropertyMap

class WrappedDiagrams[T](val backend: Layouts[T]):
  val unitWidth = backend.Layout.unitWidth

  trait WrappedDiagramFields extends AlignedDiagrams.AlignedDiagramFields:
    def toWrappedDiagram: WrappedDiagram
    def toDirectedDiagram = toAlignedDiagram.toDirectedDiagram

  sealed trait ContentWidthFields:
    val minContent: Double
    val maxContent: Double
    val numRows: NumRows

  trait Alignable:
    def toAlignedDiagram: AlignedDiagrams.AlignedDiagram

  sealed trait WrappedDiagram extends WrappedDiagramFields, ContentWidthFields, Alignable:
    def toWrappedDiagram = this

  sealed trait LocallyWrappedDiagram extends WrappedDiagram
  sealed trait GlobalWrap extends ContentWidthFields, Alignable

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

  case class Space(direction: Direction, numRows: NumRows, verticalSide: Side)
      extends LocallyWrappedDiagram, GlobalWrap:
    val properties = PropertyMap(Seq())
    val classes = Set()
    val id = None
    val minContent = 2*unitWidth
    val maxContent = minContent
    def toAlignedDiagram = AlignedDiagrams.Space(direction, verticalSide)


  case class BlockVerticalConcatenation(
      topSubdiagram: LocallyWrappedDiagram | GlobalWrap,
      bottomSubdiagram: LocallyWrappedDiagram | GlobalWrap,
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
      initNumRows: Option[NumRows],
      classes: Set[String] = Set.empty,
      id: Option[String] = None) extends SequenceWrap[D]:
    val minGap = if subdiagrams.isEmpty then 0 else ((subdiagrams.count(!_.isInstanceOf[Space]) - 1)
      * properties.get(LayoutStylesheets.Gap))
    val minContent = subdiagrams.map(_.minContent).sum + minGap
    val maxContent = subdiagrams.map(_.maxContent).sum + minGap
    val justifyContent = properties.get(LayoutStylesheets.JustifyContent)
    val numRows = initNumRows.getOrElse(NumRows.forEach(s =>
      if justifyContent.flush(s, direction) then
        (s match { case Side.Left => subdiagrams.head; case _ => subdiagrams.last }).numRows(s)
      else 1))


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
    val markerWidth = 3*backend.Layout.unitWidth

    val extraWidths = backend.BlockedHorizontalConcatenation.extraWidths(direction, tipSpecs)
    val extraWidth = extraWidths.left + extraWidths.right
    val (first, mids, last) = splitEnds(subdiagrams)
    val minContent = (List(first, last).map(_.minContent + markerWidth)
                      ++ mids.map(_.minContent + 2*markerWidth)).max + extraWidth
    val maxContent = (List(first, last).map(_.maxContent + markerWidth)
                      ++ mids.map(_.maxContent + 2*markerWidth)).max + extraWidth


  case class GlobalSequenceWrap(sw: SequenceWrap[GlobalWrap],
                                origAligned: AlignedDiagrams.Sequence) extends GlobalWrap:
    val minContent = sw.minContent
    val maxContent = sw.maxContent
    val numRows = sw.numRows
    def toAlignedDiagram = origAligned



  sealed trait HasBestUnderWidth[T]:
    def bestUnder(width: Double, depth: Int): T


  case class GloballyWrappedDiagram(
      direction: Direction,
      properties: PropertyMap,
      options: Vector[GlobalWrap],
      classes: Set[String],
      id: Option[String]) extends HasBestUnderWidth[GlobalWrap], WrappedDiagram:

    def toAlignedDiagram = options.head.toAlignedDiagram
    val numRows = options.head.numRows

    val minContentOption = options.minBy(_.minContent)
    val minContent: Double = minContentOption.minContent
    val maxContent: Double = options.map(_.maxContent).max

    def bestUnder(width: Double, depth: Int): GlobalWrap =
      options
        .filter(_.minContent <= width)
        .maxByOption(_.maxContent)
        .getOrElse(minContentOption)


  case class LocallyWrappedSequence[D <: LocallyWrappedDiagram | GlobalWrap](
        subdiagramsOne: Seq[D],
        subdiagramsMulti: Seq[D],
        direction: Direction,
        properties: PropertyMap,
        tipSpecs: TipSpecifications,
        numRows: NumRows,
        classes: Set[String] = Set.empty,
        id: Option[String] = None)
      extends HasBestUnderWidth[SequenceWrap[D]], LocallyWrappedDiagram:

    val options: Vector[(SequenceWrap[D], PartitionIndices)] =
      val (maybeSpaces, withoutSpaces) = trimSides(subdiagramsMulti, { case sp: Space => sp })
      allPartitions(withoutSpaces.toList).map((partition, indices) =>
        if partition.length == 1 then
          (HorizontalConcatenation(
             subdiagramsOne, direction, properties, Some(numRows), classes, id),
           indices)
        else
          val (first, mids, last) = splitEnds(partition)
          val leftSpace = Space(direction, NumRows(1, 1), Side.Left).asInstanceOf[D]
          val rightSpace = Space(direction, NumRows(1, 1), Side.Right).asInstanceOf[D]
          val withSpaces = ((maybeSpaces.left.toList ++ first) :+ rightSpace)
            +: mids.map(m => leftSpace +: m :+ rightSpace)
            :+ (leftSpace +: (last ++ maybeSpaces.right.toList))
          val spaceBetweenProperties =
            import LayoutStylesheets.{JustifyContent, Property}
            import JustifyContentPolicy.*
            if properties.get(JustifyContent) == SpaceBetween
            then properties.addAlways(Property(JustifyContent, Start))
                 +: List.fill(mids.length)(properties)
                 :+ properties.addAlways(Property(JustifyContent, End))
            else List.fill(withSpaces.length)(properties)
          (InlineVerticalConcatenation(
             direction.reverse(withSpaces.zip(spaceBetweenProperties).map((rowSubs, props) =>
               HorizontalConcatenation(
                 rowSubs, direction, props, None, classes, None))),
             direction, properties, tipSpecs,
             NumRows.apply.tupled(direction.swap((numRows.left, numRows.right))), classes, id),
           indices))

    val minContentOption = options.minBy(_._1.minContent)
    val minContent: Double = minContentOption._1.minContent
    val maxContent: Double = options.last._1.maxContent

    def toAlignedDiagram =
      AlignedDiagrams.Sequence(
        subdiagramsOne.map(_.toAlignedDiagram),
        subdiagramsMulti.map(_.toAlignedDiagram),
        direction, properties, tipSpecs, classes, id)

    def bestUnder(width: Double, depth: Int): SequenceWrap[D] =
      val fitting = options.filter(_._1.minContent <= width)
      // make wrapping magic happen here
      val minWrapLength = fitting.map(_._2.length).min
      // fitting
      //   .filter(_._2.length == minWrapLength)
      //   .minByOption(_._1.maxContent)
      //   .getOrElse(minContentOption)._1
      fitting
        .filterMinBy(w =>
          val wrapPenalty = w._2.length * Math.pow(2, 2*depth) * 10
          val contentPenalty = Math.pow(Math.max(0, w._1.maxContent - width), 2)
          wrapPenalty + contentPenalty)
        .filterMinBy(_._1.maxContent)
        .headOption
        .getOrElse(minContentOption)._1


  def wrapLocally(diagram: AlignedDiagrams.AlignedDiagram): LocallyWrappedDiagram =
    diagram match
      case AlignedDiagrams.Station(label, isTerminal, direction, properties, classes, id) =>
        Station(label, isTerminal, direction, properties, diagram.numRows,
                properties.get(LayoutStylesheets.Font), classes, id)
      case AlignedDiagrams.Space(direction, verticalSide) =>
        Space(direction, diagram.numRows, verticalSide)
      case AlignedDiagrams.Sequence(subdiagramsOne, subdiagramsMulti,
          direction, properties, tipSpecs, classes, id) =>
        LocallyWrappedSequence(subdiagramsOne.map(wrapLocally), subdiagramsMulti.map(wrapLocally),
          direction, properties, tipSpecs, diagram.numRows, classes, id)
      case AlignedDiagrams.BlockVerticalConcatenation(topSubdiagram, bottomSubdiagram,
          direction, polarity, properties, tipSpecs, classes, id) =>
        BlockVerticalConcatenation(wrapLocally(topSubdiagram), wrapLocally(bottomSubdiagram),
          direction, polarity, properties, tipSpecs, diagram.numRows, classes, id)


  def wrapGlobally(diagram: AlignedDiagrams.AlignedDiagram): GloballyWrappedDiagram =
    def rec(diagram: AlignedDiagrams.AlignedDiagram): Vector[GlobalWrap] =
      diagram match
        case AlignedDiagrams.Station(label, isTerminal, direction, properties, classes, id) =>
          Vector(Station(label, isTerminal, direction, properties, diagram.numRows,
                         properties.get(LayoutStylesheets.Font), classes, id))
        case AlignedDiagrams.Space(direction, verticalSide) =>
          Vector(Space(direction, diagram.numRows, verticalSide))
        case seq @ AlignedDiagrams.Sequence(subdiagramsOne, subdiagramsMulti,
            direction, properties, tipSpecs, classes, id) =>
          subdiagramsOne.zip(subdiagramsMulti)
            .map(s => rec(s._1).zip(rec(s._2)))
            .foldLeft[Vector[Vector[(GlobalWrap, GlobalWrap)]]](Vector(Vector()))((acc, subOptions) =>
              acc.flatMap(accOption => subOptions.map(accOption :+ _)))
            .map(_.unzip)
            .flatMap((so, sm) =>
              LocallyWrappedSequence(so, sm, direction, properties, tipSpecs, diagram.numRows,
                                     classes, id)
                .options.map(_._1))
            .map(sw => GlobalSequenceWrap(sw, seq))
        case AlignedDiagrams.BlockVerticalConcatenation(topSubdiagram, bottomSubdiagram,
            direction, polarity, properties, tipSpecs, classes, id) =>
          val topGlobals = rec(topSubdiagram)
          val bottomGlobals = rec(bottomSubdiagram)
          for
            tg <- topGlobals
            bg <- bottomGlobals
          yield
            BlockVerticalConcatenation(tg, bg, direction, polarity, properties, tipSpecs,
                                       diagram.numRows, classes, id)
    GloballyWrappedDiagram(diagram.direction, diagram.properties, rec(diagram),
      diagram.classes, diagram.id)
