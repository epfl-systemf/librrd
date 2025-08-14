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
            Property(name, snvs.sortBy(_._1).head._3.asInstanceOf[name.Value]))
          .toSeq)

  case class Rule(selectors: Seq[Selector], properties: Seq[Property])


  sealed trait Selector:
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]): Boolean
    type Weight = (Int, Int, Int)
    extension (w: Weight)
      def +(ow: Weight): Weight = (w._1 + ow._1, w._2 + ow._2, w._3 + ow._3)
    val weight: Weight

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
    val weight = ancestor.weight + self.weight

  case class Child(parent: Selector, self: Selector) extends Selector:
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]) =
      self.matches(selfInfo, parents)
        && !parents.isEmpty && parent.matches(parents.head, parents.tail)
    val weight = parent.weight + self.weight

  case object Wildcard extends Selector:
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]) = true
    val weight = (0, 0, 0)

  given Ordering[Selector] = Ordering.by((s: Selector) => s.weight).reverse


  sealed trait PropertyName:
    type Value
    val default: Value
    val inherited: Boolean = true
    def check(value: Value): Unit = ()

  case object AlignItems extends PropertyName:
    type Value = AlignItemsPolicy
    val default = AlignItemsPolicy.Top
  // TODO: needs to be left/right
  case object AlignSelf extends PropertyName:
    type Value = Option[AlignItemsPolicy]
    val default = None
    override val inherited = false
  case object JustifyContent extends PropertyName:
    type Value = JustifyContentPolicy
    val default = JustifyContentPolicy.SpaceBetween
  case object FlexAbsorb extends PropertyName:
    type Value = Double
    val default = 0
    override def check(value: Value) =
      assert(0 <= value && value <= 1, "flex-absorb value must be in [0, 1]")
  case object Gap extends PropertyName:
    type Value = Double
    val default = 0
    override def check(value: Value) =
      assert(0 <= value, "gap value must be nonnegative")
  case object ContinuationMarker extends PropertyName:
    type Value = String
    val default = "…"

  class Property(val name: PropertyName, val value: name.Value):
    name.check(value)
  object Property:
    def unapply(property: Property) = (property.name, property.value)

  class PropertyMap(properties: Seq[Property]):

    def get(name: PropertyName): name.Value =
      properties.collectFirst({ case Property(`name`, value: name.Value) => value })
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
