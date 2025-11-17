package ui

import org.scalajs.dom
import org.scalajs.dom.document
import librrd.SBlockLayoutsSVG.*
import librrd.{Direction, FontInfo, TipSpecifications, Side, Polarity}
import librrd.TipSpecification.*

@scalajs.js.annotation.JSExportTopLevel("SBlocksTestGUI", "gui-s-blocks-test")
object SBlocksTestGUI:
  lazy val outputCanvas = document.getElementById("output-canvas")

  val adjusted1 = HorizontalConcatenation.adjustHeights(List(
        Station("aa", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("b", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "3em")),
        Space(2*Layout.unitWidth, Direction.RTL, Side.Left),
        LineBreak(100, Direction.RTL),
        Space(2*Layout.unitWidth, Direction.RTL, Side.Right),
        Station("c", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "1em")),
        Space(2*Layout.unitWidth, Direction.RTL, Side.Left),
        LineBreak(100, Direction.RTL),
        Space(2*Layout.unitWidth, Direction.RTL, Side.Right),
        Station("ccc", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("dddd", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "2em")),
        Station("eeeee", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "1em")),
        Space(2*Layout.unitWidth, Direction.RTL, Side.Left),
      ))
  val adjusted2 = HorizontalConcatenation.adjustHeights(List(
        Station("a", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "1em")),
        HorizontalConcatenation(adjusted1, TipSpecifications(Vertical, Logical(1))),
        LineBreak(100, Direction.RTL),
        Space(2*Layout.unitWidth, Direction.RTL, Side.Right),
        Station("c", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("d", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "3em")),
        Station("f", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "3em")),
        Station("g", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("h", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "1em")),
        Station("e", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "2em")),
      ))
  val adjusted3 = HorizontalConcatenation.adjustHeights(List(
        Space(2*Layout.unitWidth, Direction.RTL, Side.Right),
        Rail(180.8666, Direction.RTL),
        Station("d", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "3em")),
        Space(2*Layout.unitWidth, Direction.RTL, Side.Left),
        LineBreak(200, Direction.RTL),
        Space(2*Layout.unitWidth, Direction.RTL, Side.Right),
        HorizontalConcatenation(adjusted2, TipSpecifications(Logical(1), Logical(1))),
        Station("e", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "2em")),
        Space(2*Layout.unitWidth, Direction.RTL, Side.Left),
      ))
  val testLayout: Layout =
    // BlockedHorizontalConcatenation(TipSpecifications(Physical(0), Physical(1)),
    //   HorizontalConcatenation(adjusted3, TipSpecifications(Vertical, Vertical)))
    HorizontalConcatenation(HorizontalConcatenation.adjustHeights(List(
      VerticalConcatenation(
        VerticalConcatenation(
          HorizontalConcatenation(List(
            Space(2*Layout.unitWidth, Direction.LTR, Side.Left),
            Station("d", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
            Space(2*Layout.unitWidth, Direction.LTR, Side.Right)),
            TipSpecifications(Vertical, Vertical)),
          HorizontalConcatenation(List(
            Space(2*Layout.unitWidth, Direction.LTR, Side.Left),
            Station("d", true, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
            Space(2*Layout.unitWidth, Direction.LTR, Side.Right)),
            TipSpecifications(Vertical, Vertical)),
          Direction.LTR,
          Polarity.+,
          TipSpecifications(Vertical, Vertical)),
        HorizontalConcatenation(List(
          Space(2*Layout.unitWidth, Direction.RTL, Side.Right),
          // Rail(14, Direction.LTR),
          Station("d", true, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "1em")),
          Space(2*Layout.unitWidth, Direction.RTL, Side.Left)),
          TipSpecifications(Vertical, Vertical)),
        Direction.LTR,
        Polarity.-,
        TipSpecifications(Logical(2), Physical(0))),
      Station("aaaa", false, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")),
      Space(2*Layout.unitWidth, Direction.LTR, Side.Right),
      LineBreak(100, Direction.LTR),
      Space(2*Layout.unitWidth, Direction.LTR, Side.Left),
      Station("aaaa", false, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em")))),
    TipSpecifications(Logical(1), Logical(1)))


  @scalajs.js.annotation.JSExport
  def init(): Unit =
    document.addEventListener("DOMContentLoaded", (event) => {
      val svg = render(testLayout).render
      outputCanvas.appendChild(svg)
    })
