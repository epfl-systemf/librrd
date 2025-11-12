package ui

import org.scalajs.dom
import org.scalajs.dom.document
import librrd.SBlockLayoutsSVG.*
import librrd.{Direction, FontInfo, TipSpecifications}
import librrd.TipSpecification.*

@scalajs.js.annotation.JSExportTopLevel("SBlocksTestGUI", "gui-s-blocks-test")
object SBlocksTestGUI:
  lazy val outputCanvas = document.getElementById("output-canvas")

  val adjusted1 = HorizontalConcatenation.adjustHeights(List(
        Station("a", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("b", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "3em")),
        LineBreak(200, Direction.LTR),
        Station("c", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("d", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "3em")),
        Station("e", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "2em")),
        Station("i", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "2em")),
      ))
  val adjusted2 = HorizontalConcatenation.adjustHeights(List(
        Station("a", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
        HorizontalConcatenation(adjusted1, TipSpecifications(Logical(1), Logical(1))),
        LineBreak(100, Direction.LTR),
        Station("c", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("d", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "3em")),
        Station("f", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "3em")),
        Station("g", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("h", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("e", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "2em")),
      ))
  val adjusted3 = HorizontalConcatenation.adjustHeights(List(
        Station("d", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "3em")),
        LineBreak(200, Direction.LTR),
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
