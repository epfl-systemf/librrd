package ui

import util.parsing.combinator.RegexParsers
import util.{Try, Success}
import librrd.{Diagrams, LayoutStylesheets, AlignItemsPolicy, JustifyContentPolicy,
               Polarity, SidedProperty, FontInfo}

trait InputParser[T]:
  def apply(input: String): Try[T]

object DiagramParser extends RegexParsers, InputParser[Diagrams.Diagram]:
  import Diagrams.*
  def diagram: Parser[Diagram] = terminal | nonterminal | sequence | stack

  def terminalLabel: Parser[String] = ("\"" ~> """[^"]+""".r <~ "\"") | ("\'" ~> """[^']+""".r <~ "\'")
  def terminal: Parser[TerminalToken] = withClassId(terminalLabel) ^^ TerminalToken.apply
  def nonterminalLabel = ("""\[\]{2,}""".r ^^ (s => ("]".repeat(s.length - 2)): String))
    | "[" ~> """[^\]]+""".r <~ "]"
  def nonterminal: Parser[NonterminalToken] = withClassId(nonterminalLabel) ^^ NonterminalToken.apply
  def sequence: Parser[Sequence] = withClassId("(" ~> diagram.* <~ ")") ^^ Sequence.apply
  def polarity: Parser[Polarity] = ("+" | "-") ^^ { case "+" => Polarity.+; case "-" => Polarity.- }
  def stack: Parser[Stack] = withClassId("(" ~> polarity ~ diagram ~ diagram <~ ")") ^^ {
    case (pol ~ top ~ bot, c, i) => Stack(top, bot, pol, classes = c, id = i) }

  def `class`: Parser[Set[String]] = ("class" ~> "=" ~> "\"" ~> """[^ \t\n\r"]+""".r.* <~ "\"") ^^ { _.toSet }
  def `id`: Parser[Option[String]] = "id" ~> "=" ~> "\"" ~> """[^"]+""".r <~ "\"" ^^ { Some(_) }
  def withClassId[T](p: Parser[T]): Parser[(T, Set[String], Option[String])] =
      p ^^ { res => (res, Set(), None) }
    | "{" ~> p ~ `class` ~ `id`.? <~ "}" ^^ (_ match { case l ~ c ~ i => (l, c, i.flatten) })
    | "{" ~> p ~ `id` ~ `class`.? <~ "}" ^^ (_ match { case l ~ i ~ c => (l, c.getOrElse(Set()), i) })

  def apply(input: String) = parseAll(diagram, input) match
    case Success(result, _) => util.Success(result)
    case e: NoSuccess => util.Failure(RuntimeException(e.msg))


object SelectorParser extends RegexParsers, PartialFunction[String, LayoutStylesheets.Selector]:
  import LayoutStylesheets.*
  override def skipWhitespace = false

  def id: Parser[ID] = "#" ~> """[^. \t\n\r]+""".r ^^ ID.apply
  def tag: Parser[Tag] = "terminal" ^^^ Tag.TerminalToken
    | "nonterminal" ^^^ Tag.NonterminalToken
    | "sequence" ^^^ Tag.Sequence
    | "stack" ^^^ Tag.Stack
  def `class`: Parser[String] = "." ~> """[^. \t\n\r]+""".r
  def tagClassList: Parser[TagClassList] =
    ((tag ~ `class`.*) ^^ { _ match { case t ~ cs => TagClassList(Some(t), cs.toSet) } })
    | (`class`.+ ^^ (cs => TagClassList(None, cs.toSet)))
  def wildcard: Parser[Wildcard.type] = "*" ^^^ Wildcard
  def root: Parser[Root.type] = ":root" ^^^ Root
  def simple: Parser[SimpleSelector] = id | tagClassList | wildcard | root

  def compoundSeparator: Parser[Char] =
    ((whiteSpace.? ~ ">" ~ whiteSpace.?) ^^ (_ => '>'))
    | whiteSpace ^^ (_ => ' ')
  def compound: Parser[Selector] =
    (simple ~ (compoundSeparator ~ simple).+) ^^ { _ match
      case first ~ restSep => restSep.foldLeft[Selector](first)((sel, ancestor) => ancestor match
        case ancSep ~ ancSel =>
          (ancSep match { case ' ' => Descendant.apply; case '>' => Child.apply })(sel, ancSel)) }

  def selector: Parser[Selector] = compound | simple

  def apply(input: String) = parseAll(selector, input.trim()).get
  def isDefinedAt(input: String) = parseAll(selector, input.trim()).successful



object StylesheetParser extends RegexParsers, InputParser[LayoutStylesheets.Stylesheet]:
  import LayoutStylesheets.*

  def alignItemsValue: Parser[AlignItems.Value] =
      "top" ^^ (_ => AlignItemsPolicy.Top)
    | "center" ^^ (_ => AlignItemsPolicy.Center)
    | "bottom" ^^ (_ => AlignItemsPolicy.Bottom)
    | "baseline" ^^ (_ => AlignItemsPolicy.Baseline)
  def justifyContentValue : Parser[JustifyContent.Value] =
      "start" ^^ (_ => JustifyContentPolicy.Start)
    | "end" ^^ (_ => JustifyContentPolicy.End)
    | "left" ^^ (_ => JustifyContentPolicy.Left)
    | "right" ^^ (_ => JustifyContentPolicy.Right)
    | "space-between" ^^ (_ => JustifyContentPolicy.SpaceBetween)
    | "space-around" ^^ (_ => JustifyContentPolicy.SpaceAround)
    | "space-evenly" ^^ (_ => JustifyContentPolicy.SpaceEvenly)
    | "center" ^^ (_ => JustifyContentPolicy.Center)
  def otherValue: Parser[String] = """[A-Za-z0-9-.]+""".r
  def property: Parser[Property] =
     ("align-items:" ~> alignItemsValue ^^ (v => Property(AlignItems, v))
    | "align-self:" ~> alignItemsValue ~ alignItemsValue ^^ (_ match
        case vLeft ~ vRight => Property(AlignSelf, SidedProperty(vLeft, vRight)))
    | "align-self:" ~> alignItemsValue ^^ (v => Property(AlignSelf, SidedProperty(v, v)))
    | "justify-content:" ~> justifyContentValue ^^ (v => Property(JustifyContent, v))
    | "flex-absorb:" ~> """[0-9.]+""".r ^^ (v => Property(FlexAbsorb, v.toDouble))
    | "gap:" ~> """[0-9.]+""".r ^^ (v => Property(Gap, v.toDouble))
    | "continuation-marker:" ~> """"\S+"""".r ^^ (v =>
        Property(ContinuationMarker, v.substring(1, v.length() - 1)))
    | "font:" ~> """[A-Za-z0-9-]+|('[^']+')|("[^"]+")""".r ~ otherValue.? ~ otherValue.? ~ otherValue.?
      ^^ { _ match { case family ~ style ~ weight ~ size =>
      Property(Font, FontInfo(family, style.getOrElse("normal"), weight.getOrElse("normal"),
                              size.getOrElse("1rem"))) }}) <~ ";"

  def selector: Parser[Selector] = "[^,{]+".r ^? SelectorParser
  def rule: Parser[Rule] = (rep1sep(selector, ",") <~ "{") ~ property.* <~ "}"
    ^^ { _ match { case selectors ~ properties => Rule(selectors, properties) }}
  def stylesheet: Parser[Stylesheet] = rule.* ^^ (rs => Stylesheet(rs))

  def apply(input: String) = parseAll(stylesheet, input) match
    case Success(result, _) => util.Success(result)
    case e: NoSuccess => util.Failure(RuntimeException(e.msg))


object IdentityParser extends InputParser[String]:
  def apply(input: String) = Success(input)

/*
((+ ("WITH" (+ () "RECURSIVE") (- [common-table-expression] ",")) ())
      (+ "REPLACE" ("INSERT" (+ () ("OR" (+ "ABORT" (+ "FAIL" (+ "IGNORE" (+ "REPLACE" "ROLLBACK"))))))))
      "INTO"
      (+ () ("schema-name" ".")) "table-name" (+ ("AS" "alias") ())
      (+ ("(" (- "column-name" ",") ")") ())
      (+ ("VALUES" (- ("(" (- [expr] ",") ")") ",") (+ () [upsert-clause]))
          (+ ([select-stmt] (+ () [upsert-clause]))
              ("DEFAULT" "VALUES")))
      (+ () [returning-clause]))
*/
