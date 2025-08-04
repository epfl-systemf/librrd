package librrd

object LayoutStylesheets:

  enum Tag { case TerminalToken, NonterminalToken, Sequence, Stack }
  case class TagInfo(tag: Tag, id: Option[String], classes: Set[String])

  case class Stylesheet(rules: Seq[Rule]):
    def mostSpecificProperties(selfInfo: TagInfo, parents: Seq[TagInfo]) =
      PropertyMap(
        rules
          .flatMap { rule => rule.selectors
            .filter(_.matches(selfInfo, parents))
            .maxOption
            .map(s => (s, rule.properties)) }
          .flatMap(sp => sp._2.map(p => (sp._1, p.name, p.value)))
          .groupBy(_._2)
          .map((name, snvs) =>
            Property(name, snvs.sortBy(_._1).head._3.asInstanceOf[name.V]))
          .toSeq)

  case class Rule(selectors: Seq[Selector], properties: Seq[Property])


  sealed trait Selector:
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]): Boolean
    val weight: (Int, Int, Int)

  case class ID(id: String) extends Selector:
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]) = selfInfo.id.fold(false)(_ == id)
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


  sealed trait PropertyName:
    type V
    val default: V
    val inherited: Boolean = true
    def check(value: V): Unit = ()

  case object AlignItems extends PropertyName:
    type V = AlignItemsPolicy
    val default = AlignItemsPolicy.Top
  // TODO: needs to be left/right
  case object AlignSelf extends PropertyName:
    type V = Option[AlignItemsPolicy]
    val default = None
    override val inherited = false
  case object JustifyContent extends PropertyName:
    type V = JustifyContentPolicy
    val default = JustifyContentPolicy.SpaceBetween
  case object FlexAbsorb extends PropertyName:
    type V = Double
    val default = 0
    override def check(value: V) =
      assert(0 <= value && value <= 1, "flex-absorb value must be in [0, 1]")
  case object Gap extends PropertyName:
    type V = Double
    val default = 0
    override def check(value: V) =
      assert(0 <= value, "gap value must be nonnegative")

  class Property(val name: PropertyName, val value: name.V):
    name.check(value)
  object Property:
    def unapply(property: Property) = (property.name, property.value)

  class PropertyMap(properties: Seq[Property]):

    def get(name: PropertyName): name.V =
      properties.collectFirst({ case Property(`name`, v: name.V) => v })
        .getOrElse(name.default)

    def resolveStartEnd(direction: Direction): PropertyMap =
      import Direction.*
      import JustifyContentPolicy.*
      (get(JustifyContent), direction) match
        case (Start, LTR) | (End, RTL) =>
          PropertyMap(Property(JustifyContent, Left) +: properties)
        case (End, LTR) | (Start, RTL) =>
          PropertyMap(Property(JustifyContent, Right) +: properties)
        case _ => this
