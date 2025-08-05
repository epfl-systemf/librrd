package librrd

import LayoutStylesheets.PropertyMap

class WrappedDiagrams[T](val backend: Layouts[T]):
  val unitWidth = backend.Layout.unitWidth

  trait WrappedDiagramFields extends AlignedDiagrams.AlignedDiagramFields:
    def toWrappedDiagram: WrappedDiagram
    def toDirectedDiagram = toAlignedDiagram.toDirectedDiagram

  sealed trait WrappedDiagram extends WrappedDiagramFields:
    val minContent: Double
    val maxContent: Double
    def toWrappedDiagram = this

  sealed trait LocallyWrappedDiagram extends WrappedDiagram
  sealed trait GlobalWrap

  case class Station(label: String,
                     isTerminal: Boolean,
                     direction: Direction,
                     properties: PropertyMap,
                     classes: Set[String] = Set.empty,
                     id: Option[String] = None) extends LocallyWrappedDiagram, GlobalWrap:
    val minContent = backend.measure(label)._1
    val maxContent = minContent
    def toAlignedDiagram =
      AlignedDiagrams.Station(label, isTerminal, direction, properties, classes, id)

  case class Space(direction: Direction) extends LocallyWrappedDiagram, GlobalWrap:
    val properties = PropertyMap(Seq())
    val classes = Set()
    val id = None
    val minContent = unitWidth
    val maxContent = minContent
    def toAlignedDiagram = AlignedDiagrams.Space(direction)


  case class BlockVerticalConcatenation(
      topSubdiagram: WrappedDiagram,
      bottomSubdiagram: WrappedDiagram,
      direction: Direction,
      polarity: Polarity,
      properties: PropertyMap,
      tipSpecs: TipSpecifications,
      classes: Set[String] = Set.empty,
      id: Option[String] = None) extends LocallyWrappedDiagram, GlobalWrap:

    def content(accessor: WrappedDiagram => Double): Double =
      Math.max(accessor(topSubdiagram), accessor(bottomSubdiagram))
      + Side.values.map(s => (tipSpecs(s), polarity) match
          case (TipSpecification.Vertical, Polarity.+) => 0
          case (TipSpecification.Vertical
              | TipSpecification.Logical(1)
              | TipSpecification.Physical(0), Polarity.-) => 2 * unitWidth
          case _ => 3 * unitWidth).sum
    val minContent = content(_.minContent)
    val maxContent = content(_.maxContent)

    def toAlignedDiagram = AlignedDiagrams.BlockVerticalConcatenation(
      topSubdiagram.toAlignedDiagram, bottomSubdiagram.toAlignedDiagram,
      direction, polarity, properties, tipSpecs, classes, id)


  sealed trait SequenceWrap[D]
  case class HorizontalConcatenation[D](
    subdiagrams: Seq[D | InlineVerticalConcatenation[D]],
    direction: Direction,
    properties: PropertyMap,
    classes: Set[String] = Set.empty,
    id: Option[String] = None) extends SequenceWrap[D]
  case class InlineVerticalConcatenation[D](
    subdiagrams: Seq[D | HorizontalConcatenation[D]],
    direction: Direction,
    properties: PropertyMap,
    tipSpecs: TipSpecifications,
    classes: Set[String] = Set.empty,
    id: Option[String] = None) extends SequenceWrap[D]

  case class GlobalSequenceWrap(sw: SequenceWrap[GlobalWrap]) extends GlobalWrap

  sealed trait HasBestUnderWidth[T]:
    val options: Iterable[T]
    def bestUnder(width: Double): T


  case class GloballyWrappedDiagram(
      val direction: Direction,
      val properties: PropertyMap,
      val options: Iterable[GlobalWrap]) extends HasBestUnderWidth[GlobalWrap], WrappedDiagram:

    val classes: Set[String] = ???
    val id: Option[String] = ???

    val maxContent: Double = ???
    val minContent: Double = ???
    
    def toAlignedDiagram: librrd.AlignedDiagrams.AlignedDiagram = ???

    def bestUnder(width: Double): WrappedDiagrams.this.GlobalWrap = ???


  case class LocallyWrappedSequence(
        subdiagrams: Seq[WrappedDiagram],
        direction: Direction,
        properties: PropertyMap,
        tipSpecs: TipSpecifications,
        classes: Set[String] = Set.empty,
        id: Option[String] = None)
      extends HasBestUnderWidth[SequenceWrap[LocallyWrappedDiagram]], LocallyWrappedDiagram:

    val options: Iterable[SequenceWrap[LocallyWrappedDiagram]] = ???
    val maxContent: Double = ???
    val minContent: Double = ???

    def toAlignedDiagram: librrd.AlignedDiagrams.AlignedDiagram = ???

    def bestUnder(width: Double): SequenceWrap[LocallyWrappedDiagram] = ???
