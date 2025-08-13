package librrd

import util.parsing.combinator.RegexParsers
import util.{Try, Success}

trait InputParser[T]:
  def apply(input: String): Try[T]

object DiagramParser extends RegexParsers, InputParser[Diagrams.Diagram]:
  import Diagrams.*
  def diagram: Parser[Diagram] = terminal | nonterminal | sequence | stack
  def terminal: Parser[TerminalToken] =
    (("\"" ~> """[^"]+""".r <~ "\"") | ("\'" ~> """[^']+""".r <~ "\'")) ^^ (l => TerminalToken(l))
  def nonterminal: Parser[NonterminalToken] =
    "[" ~> """[^\]]+""".r <~ "]" ^^ (l => NonterminalToken(l))
  def sequence: Parser[Sequence] = "(" ~> diagram.* <~ ")" ^^ (ds => Sequence(ds))
  def polarity: Parser[Polarity] = ("+" | "-") ^^ { case "+" => Polarity.+; case "-" => Polarity.- }
  def stack: Parser[Stack] = "(" ~> polarity ~ diagram ~ diagram <~ ")" ^^ {
    case pol ~ top ~ bot => Stack(top, bot, pol) }
  def apply(input: String) = parseAll(diagram, input) match
    case Success(result, _) => util.Success(result)
    case e: NoSuccess => util.Failure(RuntimeException(e.msg))


object StylesheetParser extends RegexParsers, InputParser[LayoutStylesheets.Stylesheet]:
  import LayoutStylesheets.*

  def id: Parser[ID] = "#" ~> """\S+""".r ^^ ID.apply
  def tag: Parser[Tag] = "terminal" ^^ (_ => Tag.TerminalToken)
    | "nonterminal" ^^ (_ => Tag.NonterminalToken)
    | "sequence" ^^ (_ => Tag.Sequence)
    | "stack" ^^ (_ => Tag.Stack)
  def `class`: Parser[String] = "." ~> """[^.]+""".r
  def tagClassList: Parser[TagClassList] =
    ((tag ^^ (t => Some(t))) ~ `class`.* | tag.? ~ `class`.+) ^^ { _ match
      case maybeTag ~ classes => TagClassList(maybeTag, classes.toSet) }
  def wildcard: Parser[Selector] = "*" ^^ (_ => Wildcard)
  def atomic: Parser[Selector] = id | tagClassList | wildcard
  def descendant: Parser[Descendant] =
    (atomic <~ whiteSpace) ~ atomic ^^ { s => Descendant(s._1, s._2) }
  def child : Parser[Child] = (atomic <~ ">") ~ atomic ^^ { s => Child(s._1, s._2) }
  def selector: Parser[Selector] = descendant | child | atomic

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
  def property: Parser[Property] =
     ("align-items:" ~> alignItemsValue ^^ (v => Property(AlignItems, v))
    | "align-self:" ~> alignItemsValue ^^ (v => Property(AlignSelf, Some(v)))
    | "justify-content:" ~> justifyContentValue ^^ (v => Property(JustifyContent, v))
    | "flex-absorb:" ~> """[0-9.]+""".r ^^ (v => Property(FlexAbsorb, v.toDouble))
    | "gap:" ~> """[0-9.]+""".r ^^ (v => Property(Gap, v.toDouble))
    | "continuation-marker:" ~> """"\S+"""".r ^^ (v =>
        Property(ContinuationMarker, v.substring(1, v.length() - 1)))) <~ ";"

  def rule: Parser[Rule] = (rep1sep(selector, ",") <~ "{") ~ property.* <~ "}" ^^ { _ match
    case selectors ~ properties => Rule(selectors, properties) }
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
