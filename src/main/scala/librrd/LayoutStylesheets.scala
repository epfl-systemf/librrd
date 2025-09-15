package librrd

object LayoutStylesheets:

  enum Tag { case TerminalToken, NonterminalToken, Sequence, Stack }
  case class TagInfo(tag: Tag, id: Option[String], classes: Set[String])

  case class Stylesheet(rules: Seq[Rule]):
    def mostSpecificProperties(selfInfo: TagInfo, parents: Seq[TagInfo],
                               inheritable: PropertyMap) =
      PropertyMap()
        .addAllUnlessExists(
          rules
            .flatMap { rule =>
              rule.selectors
                .filter(_.matches(selfInfo, parents))
                .maxOption
                .map(s => (s, rule.properties))
              }
            .reverse
            .sortBy(_._1)
            .flatMap(_._2))
        .addAllUnlessExists(inheritable)
        .addUnlessExists(Property(
          AlignSelf, SidedProperty.forEach(_ => inheritable.get(AlignItems))))

  case class Rule(selectors: Seq[Selector], properties: Seq[Property])


  sealed trait Selector:
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]): Boolean
    type Weight = (Int, Int, Int)
    extension (w: Weight)
      def +(ow: Weight): Weight = (w._1 + ow._1, w._2 + ow._2, w._3 + ow._3)
    val weight: Weight

  sealed trait SimpleSelector extends Selector

  case class ID(id: String) extends SimpleSelector:
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]) = selfInfo.id.fold(false)(_ == id)
    val weight = (1, 0, 0)

  case class TagClassList(tag: Option[Tag], classes: Set[String]) extends SimpleSelector:
    assert(tag.isDefined || !classes.isEmpty,
      "must specify at least either tag or class in selector")
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]) =
      tag.fold(true)(_ == selfInfo.tag) && classes.subsetOf(selfInfo.classes)
    val weight = (0, classes.size, tag.fold(0)(_ => 1))

  case object Wildcard extends SimpleSelector:
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]) = true
    val weight = (0, 0, 0)

  case object Root extends SimpleSelector:
    def matches(selfInfo: TagInfo, parents: Seq[TagInfo]) = parents.isEmpty
    val weight = (0, 1, 0)

  case class Descendant(ancestor: Selector, self: SimpleSelector) extends Selector:
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

  given Ordering[Selector] = Ordering.by((s: Selector) => s.weight).reverse


  sealed trait PropertyName:
    type Value
    val default: Value
    val inheritable: Boolean = false
    def check(value: Value): Unit = ()

  case object AlignItems extends PropertyName:
    type Value = AlignItemsPolicy
    val default = AlignItemsPolicy.Top
    override val inheritable = true
  case object AlignSelf extends PropertyName:
    type Value = SidedProperty[AlignItemsPolicy]
    val default = SidedProperty(AlignItems.default, AlignItems.default)
  case object JustifyContent extends PropertyName:
    type Value = JustifyContentPolicy
    val default = JustifyContentPolicy.SpaceBetween
    override val inheritable = true
  case object FlexAbsorb extends PropertyName:
    type Value = Double
    val default = 0
    override val inheritable = true
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
  case object Font extends PropertyName:
    type Value = FontInfo
    val default = FontInfo("sans-serif", "normal", "normal", "14px")

  class Property(val name: PropertyName, val value: name.Value):
    name.check(value)
  object Property:
    def unapply(property: Property) = (property.name, property.value)

  class PropertyMap(private val properties: Vector[Property]):
    def addAlways(property: Property) = new PropertyMap(property +: properties)
    def addAllAlways(newProperties: Seq[Property]) =
      new PropertyMap(PropertyMap.compact(newProperties.toVector.reverse ++ properties))
    def addAllAlways(other: PropertyMap) =
      new PropertyMap(PropertyMap.compact(other.properties.reverse ++ properties))
    def addUnlessExists(property: Property) = new PropertyMap(properties :+ property)
    def addAllUnlessExists(newProperties: Seq[Property]) =
      new PropertyMap(PropertyMap.compact(properties ++ newProperties.toVector))
    def addAllUnlessExists(other: PropertyMap) =
      new PropertyMap(PropertyMap.compact(properties ++ other.properties))
    def get(name: PropertyName): name.Value = getOption(name).getOrElse(name.default)
    def getOption(name: PropertyName): Option[name.Value] =
      properties.collectFirst({ case Property(`name`, value: name.Value) => value })
    def filterInheritable = new PropertyMap(properties.filter(_.name.inheritable))

  object PropertyMap:
    def apply() = new PropertyMap(Vector())
    def apply(properties: Seq[Property]) = new PropertyMap(properties.toVector)
    private def compact(properties: Vector[Property]) = properties.distinctBy(_.name)

  val defaultProperties =
    PropertyMap(List(AlignItems, AlignSelf, JustifyContent, FlexAbsorb, Gap, ContinuationMarker, Font)
      .map(pn => Property(pn, pn.default)))
