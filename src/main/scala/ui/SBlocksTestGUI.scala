package ui

import org.scalajs.dom
import org.scalajs.dom.document
import librrd.SBlockLayoutsSVG.*
import librrd.{Direction, FontInfo, TipSpecifications, Side}
import librrd.TipSpecification.*

@scalajs.js.annotation.JSExportTopLevel("SBlocksTestGUI", "gui-s-blocks-test")
object SBlocksTestGUI:
  lazy val outputCanvas = document.getElementById("output-canvas")

  val adjusted1 = HorizontalConcatenation.adjustHeights(List(
        Station("aa", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("b", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "3em")),
        Space(2*Layout.unitWidth, Direction.LTR, Side.Right),
        LineBreak(100, Direction.LTR),
        Space(2*Layout.unitWidth, Direction.LTR, Side.Left),
        Station("c", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
        Space(2*Layout.unitWidth, Direction.LTR, Side.Right),
        LineBreak(100, Direction.LTR),
        Space(2*Layout.unitWidth, Direction.LTR, Side.Left),
        Station("ccc", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("dddd", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "2em")),
        Station("eeeee", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
        Space(2*Layout.unitWidth, Direction.LTR, Side.Right),
      ))
  val adjusted2 = HorizontalConcatenation.adjustHeights(List(
        Station("a", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
        HorizontalConcatenation(adjusted1, TipSpecifications(Logical(1), Vertical)),
        LineBreak(100, Direction.LTR),
        Space(2*Layout.unitWidth, Direction.LTR, Side.Left),
        Station("c", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("d", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "3em")),
        Station("f", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "3em")),
        Station("g", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("h", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("e", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "2em")),
      ))
  val adjusted3 = HorizontalConcatenation.adjustHeights(List(
        Station("d", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "3em")),
        Space(2*Layout.unitWidth, Direction.LTR, Side.Right),
        LineBreak(200, Direction.LTR),
        Space(2*Layout.unitWidth, Direction.LTR, Side.Left),
        HorizontalConcatenation(adjusted2, TipSpecifications(Logical(1), Logical(1))),
        Station("e", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "2em")),
      ))
  val testLayout: Layout =
    HorizontalConcatenation(adjusted3, TipSpecifications(Logical(1), Logical(1)))


  @scalajs.js.annotation.JSExport
  def init(): Unit =
    document.addEventListener("DOMContentLoaded", (event) => {
      val svg = render(testLayout).render
      outputCanvas.appendChild(svg)
    })
