package librrd

object LayoutStylesheets:

  enum Tag { case TerminalToken, NonterminalToken, Sequence, Stack }
  case class TagInfo(tag: Tag, id: String, classes: Set[String])

  case class Stylesheet(rules: Seq[Rule]):
    def mostSpecificProperties(selfInfo: TagInfo, parents: Seq[TagInfo]) =
      rules
        .flatMap { rule => rule.selectors
          .filter(_.matches(selfInfo, parents))
          .maxOption
          .map(s => (s, rule.properties)) }
        .flatMap(sp => sp._2.map((sp._1, _)))
        .groupMapReduce(_._2)(_._1)(summon[Ordering[Selector]].min)

  case class Rule(selectors: Seq[Selector], properties: Seq[Property])


  sealed trait Selector:
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]): Boolean
    val weight: (Int, Int, Int)

  case class ID(id: String) extends Selector:
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]) = selfInfo.id == id
    val weight = (1, 0, 0)

  case class TagClassList(tag: Option[Tag], classes: Set[String]) extends Selector:
    assert(tag.isDefined || !classes.isEmpty,
      "must specify at least either tag or class in selector")
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]) =
      tag.fold(true)(_ == selfInfo.tag) && classes.subsetOf(selfInfo.classes)
    val weight = (0, classes.size, tag.fold(0)(_ => 1))

  case class Descendant(ancestor: Selector, self: Selector) extends Selector:
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]) =
      self.matches(selfInfo, parents)
        && parents.tails.filterNot(_.isEmpty)
             .exists(ps => ancestor.matches(ps.head, ps.tail))
    val weight = self.weight

  case class Child(parent: Selector, self: Selector) extends Selector:
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]) =
      self.matches(selfInfo, parents)
        && !parents.isEmpty && parent.matches(parents.head, parents.tail)
    val weight = self.weight

  case object Wildcard extends Selector:
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]) = true
    val weight = (0, 0, 0)

  given Ordering[Selector] = Ordering.by((s: Selector) => s.weight).reverse


  sealed trait Property:
    type V
    val value: V
    val inherited: Boolean = true

  case class AlignItems(value: AlignItemsPolicy) extends Property:
    type V = AlignItemsPolicy
  case class AlignSelf(value: AlignItemsPolicy) extends Property:
    type V = AlignItemsPolicy
    override val inherited = false
  case class JustifyContent(value: JustifyContentPolicy) extends Property:
    type V = JustifyContentPolicy
  case class FlexAbsorb(value: Double) extends Property:
    type V = Double
    assert(0 <= value && value <= 1, "flex-absorb value must be in [0, 1]")
  case class Gap(value: Double) extends Property:
    type V = Double
    assert(0 <= value, "gap value must be nonnegative")
