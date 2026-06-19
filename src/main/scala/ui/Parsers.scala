package ui

import util.parsing.combinator.RegexParsers
import util.{Try, Success}
import librrd.*

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
  def polarity: Parser[PrePolarity] = ("+" | "-?" | "-!" | "-") ^^ {
    case "+" => PrePolarity.+; case "-" => PrePolarity.-;
    case "-?" => PrePolarity.-?; case "-!" => PrePolarity.-! }
  def stack: Parser[Stack] = withClassId("(" ~> polarity ~ diagram ~ diagram <~ ")") ^^ {
    case (pol ~ top ~ bot, c, i, gl) => Stack(top, bot, pol, classes = c, id = i, groupLabel = gl) }

  def `class`: Parser[Set[String]] = ("class" ~> "=" ~> "\"" ~> """[^ \t\n\r"]+""".r.* <~ "\"") ^^ { _.toSet }
  def `id`: Parser[String] = "id" ~> "=" ~> "\"" ~> """[^"]+""".r <~ "\""
  def groupLabel: Parser[String] = "label" ~> "=" ~> "\"" ~> """[^"]+""".r <~ "\""
  def withClassId[T](p: Parser[T]): Parser[(T, Set[String], Option[String], Option[String])] =
      p ^^ { res => (res, Set(), None, None) }
    | "{" ~> p ~ `class` ~ `id`.? ~ groupLabel.? <~ "}" ^^ (_ match { case l ~ c ~ i ~ gl => (l, c, i, gl) })
    | "{" ~> p ~ `class` ~ groupLabel.? ~ `id`.? <~ "}" ^^ (_ match { case l ~ c ~ gl ~ i => (l, c, i, gl) })
    | "{" ~> p ~ `id` ~ groupLabel.? ~ `class`.? <~ "}" ^^ (_ match { case l ~ i ~ gl ~ c => (l, c.getOrElse(Set()), Some(i), gl) })
    | "{" ~> p ~ `id` ~ `class`.? ~ groupLabel.? <~ "}" ^^ (_ match { case l ~ i ~ c ~ gl => (l, c.getOrElse(Set()), Some(i), gl) })
    | "{" ~> p ~ groupLabel ~ `id`.? ~ `class`.? <~ "}" ^^ (_ match { case l ~ gl ~ i ~ c => (l, c.getOrElse(Set()), i, Some(gl)) })
    | "{" ~> p ~ groupLabel ~ `class`.? ~ `id`.? <~ "}" ^^ (_ match { case l ~ gl ~ c ~ i => (l, c.getOrElse(Set()), i, Some(gl)) })

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

  def textBoxOverEdgeValue: Parser[TextBoxOverEdge] =
      "text" ^^ (_ => TextBoxOverEdge.Text)
    | "cap" ^^ (_ => TextBoxOverEdge.Cap)
    | "ex" ^^ (_ => TextBoxOverEdge.Ex)
    | "ink" ^^ (_ => TextBoxOverEdge.Ink)
  def textBoxUnderEdgeValue: Parser[TextBoxUnderEdge] =
      "text" ^^ (_ => TextBoxUnderEdge.Text)
    | "alphabetic" ^^ (_ => TextBoxUnderEdge.Alphabetic)
    | "ink" ^^ (_ => TextBoxUnderEdge.Ink)
  def textBoxTrimValue: Parser[TextBoxTrimPolicy] =
      "none" ^^ (_ => TextBoxTrimPolicy.None)
    | "trim-both" ^^ (_ => TextBoxTrimPolicy.TrimBoth)
    | "trim-start" ^^ (_ => TextBoxTrimPolicy.TrimStart)
    | "trim-end" ^^ (_ => TextBoxTrimPolicy.TrimEnd)
  def textBoxAlignValue: Parser[librrd.TextBoxAlignPolicy] =
      "baseline" ^^ (_ => librrd.TextBoxAlignPolicy.Baseline)
    | "center" ^^ (_ => librrd.TextBoxAlignPolicy.Center)
    | "bottom" ^^ (_ => librrd.TextBoxAlignPolicy.Bottom)

  def labelPositionBlockValue: Parser[LabelPositionBlock] =
      "top" ^^^ LabelPositionBlock.Top
    | "bottom" ^^^ LabelPositionBlock.Bottom
  def labelPositionInlineValue: Parser[LabelPositionInline] =
      "left" ^^^ LabelPositionInline.Left
    | "right" ^^^ LabelPositionInline.Right
    | "center" ^^^ LabelPositionInline.Center
    | "start" ^^^ LabelPositionInline.Start
    | "end" ^^^ LabelPositionInline.End

  def otherValue: Parser[String] = """[A-Za-z0-9-.]+""".r

  def property: Parser[Property] =
     ("align-items:" ~> alignItemsValue ^^ (v => Property(AlignItems, v))
    | "align-self:" ~> alignItemsValue ~ alignItemsValue ^^ (_ match
        case vLeft ~ vRight => Property(AlignSelf, SidedProperty(vLeft, vRight)))
    | "align-self:" ~> alignItemsValue ^^ (v => Property(AlignSelf, SidedProperty(v, v)))
    | "justify-content:" ~> justifyContentValue ^^ (v => Property(JustifyContent, v))
    | "flex-absorb:" ~> """[0-9.]+""".r ^^ (v => Property(FlexAbsorb, v.toDouble))
    | "gap:" ~> """[0-9.]+""".r ^^ (v => Property(Gap, v.toDouble))
    | "row-gap:" ~> """[0-9.]+""".r ^^ (v => Property(RowGap, v.toDouble))
    | "continuation-marker:" ~> (("none" ^^ (_ => Property(ContinuationMarker, None)))
      | """"\S+"""".r ^^ (v => Property(ContinuationMarker, Some(v.substring(1, v.length() - 1)))))
    | "text-box-edge:" ~> textBoxOverEdgeValue ~ textBoxUnderEdgeValue ^^ (_ match
        case over ~ under => Property(TextBoxEdge, TextBoxEdges(over, under)))
    | "text-box-trim:" ~> textBoxTrimValue ^^ (v => Property(TextBoxTrim, v))
    | "text-box-align:" ~> textBoxAlignValue ^^ (v => Property(TextBoxAlign, v))
    | "label-position:" ~> labelPositionBlockValue ~ labelPositionInlineValue ^^ (_ match
        case bv ~ iv => Property(LabelPosition, LabelPositionValue(bv, iv)))
    | "font:" ~> """[A-Za-z0-9-]+|('[^']+')|("[^"]+")""".r ~ otherValue.? ~ otherValue.? ~ otherValue.?
      ^^ { _ match { case family ~ style ~ weight ~ size =>
      Property(Font, FontInfo(family, style.getOrElse("normal"), weight.getOrElse("normal"),
                              size.getOrElse("14px"))) }}
    | "system-font:" ~> """[A-Za-z0-9-]+|('[^']+')|("[^"]+")""".r ~ otherValue.? ~ otherValue.? ~ otherValue.?
      ^^ { _ match { case family ~ style ~ weight ~ size =>
      Property(SystemFont, FontInfo(family, style.getOrElse("normal"), weight.getOrElse("normal"),
                                    size.getOrElse("14px"))) }})
     <~ ";"

  def selector: Parser[Selector] = "[^,{]+".r ^? SelectorParser
  def rule: Parser[Rule] = (rep1sep(selector, ",") <~ "{") ~ property.* <~ "}"
    ^^ { _ match { case selectors ~ properties => Rule(selectors, properties) }}
  def stylesheet: Parser[Stylesheet] = rule.* ^^ (rs => Stylesheet(rs))

  def apply(input: String) = parseAll(stylesheet, input) match
    case Success(result, _) => util.Success(result)
    case e: NoSuccess => util.Failure(RuntimeException(e.msg))


class LayoutParser[T](val backend: Layouts[T]) extends RegexParsers:
  def realNumber: Parser[Double] = """-?(\d+)?(\.\d+)?""".r ^^ { _.toDouble }
  def wholeNumber: Parser[Integer] = """-?\d+""".r ^^ { _.toInt }

  def direction: Parser[Direction] =
    "ltr" ^^ (_ => Direction.LTR) | "rtl" ^^ (_ => Direction.RTL)
  def width: Parser[Double] = realNumber.filter(_ >= 0)
  def flag: Parser[Boolean] = "#t" ^^ (_ => true) | "#f" ^^ (_ => false)
  def polarity: Parser[Polarity] =
    "+" ^^ (_ => Polarity.+) | ("-?" | "-") ^^ (_ => Polarity.-?) | ("-!") ^^ (_ => Polarity.-!)
  def rowNumber: Parser[Integer] = wholeNumber.filter(_ > 0)
  def proportion: Parser[Double] = realNumber.filter(p => 0 <= p && p <= 1)
  def side: Parser[Side] = "left" ^^ (_ => Side.Left) | "right" ^^ (_ => Side.Right)

  def tipSpecification: Parser[TipSpecification] =
      "vertical" ^^ (_ => TipSpecification.Vertical)
    | "(" ~ "logical" ~> rowNumber <~ ")" ^^ { TipSpecification.Logical(_) }
    | "(" ~ "physical" ~> proportion <~ ")" ^^ { TipSpecification.Physical(_) }

  val terminalFont = FontInfo("Inconsolata", "normal", "normal", "14px")
  val nonterminalFont = FontInfo("Linux Biolinum", "italic", "normal", "14px")
  val markerFont = FontInfo("Linux Biolinum", "normal", "bold", "14px")
  val textBoxEdges = TextBoxEdges(TextBoxOverEdge.Cap, TextBoxUnderEdge.Alphabetic)
  val textBoxTrim  = TextBoxTrimPolicy.TrimBoth
  val textBoxAlign = TextBoxAlignPolicy.Baseline

  def layout: Parser[backend.Layout] =
      ("(" ~ "rail") ~> (direction ~ width) <~ ")"
        ^^ { _ match { case d ~ w => backend.Rail(w, d) } }
    | ("(" ~ "space") ~> (direction ~ side) <~ ")"
        ^^ { _ match { case d ~ s => backend.Space(d, s) } }
    | ("(" ~ "station") ~> (direction ~ """"[^"]+"""".r ~ flag) <~ ")"
        ^^ { _ match { case d ~ l ~ t => backend.Station(l.substring(1, l.length() - 1), t, d,
          if t then terminalFont else nonterminalFont,
          textBoxEdges, textBoxTrim, textBoxAlign) } }
    | ("(" ~ "hconcat") ~> (direction ~ layout.+) <~ ")"
        ^^ { _ match { case d ~ ls => backend.HorizontalConcatenation(backend.HorizontalConcatenation.adjustHeights(ls), false) } }
    | ("(" ~ "vconcat-inline") ~>
        (direction ~ tipSpecification ~ tipSpecification ~ """"[^"]+"""".r ~ layout.+) <~ ")"
        ^^ { _ match { case d ~ lts ~ rts ~ mk ~ ls =>
          val marker = mk.substring(1, mk.length() - 1)
          val width = ls(0).width + backend.LineBreak.markerWidth(Some(marker), markerFont)
          backend.HorizontalConcatenation(
            backend.HorizontalConcatenation.adjustHeights(
              ls.flatMap(l => Seq(l, backend.LineBreak(width, d, Some(marker), markerFont,
                                                       LayoutStylesheets.RowGap.default)))
                .dropRight(1)),
            false)
        } }
    | ("(" ~ "vconcat-block") ~>
        (direction ~ tipSpecification ~ tipSpecification ~ polarity ~ layout ~ layout) <~ ")"
        ^^ { _ match { case d ~ lts ~ rts ~ pol ~ ltop ~ lbot =>
          val tipSpecs = SidedProperty(lts, rts)
          backend.VerticalConcatenation(ltop.block, lbot.block, d, pol, tipSpecs,
            LayoutStylesheets.RowGap.default) } }

  def apply(input: String) = parseAll(layout, input) match
    case Success(result, _) => util.Success(result)
    case e: NoSuccess => util.Failure(RuntimeException(e.msg))


object IdentityParser extends InputParser[String]:
  def apply(input: String) = Success(input)
