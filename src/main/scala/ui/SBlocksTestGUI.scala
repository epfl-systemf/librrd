package ui

import org.scalajs.dom
import org.scalajs.dom.document
import librrd.SBlockLayoutsSVG.*
import librrd.{Direction, FontInfo, TipSpecifications, Side, Polarity}
import librrd.TipSpecification.*

@scalajs.js.annotation.JSExportTopLevel("SBlocksTestGUI", "gui-s-blocks-test")
object SBlocksTestGUI:
  lazy val outputCanvas = document.getElementById("output-canvas")


  val thing1 =
    VerticalConcatenation(
      BlockedHorizontalConcatenation(HorizontalConcatenation(List(
          Space(2*Layout.unitWidth, Direction.LTR, Side.Left),
          Station("thing1a", true, Direction.LTR, FontInfo("monospace", "normal", "normal", "1em")),
          Space(2*Layout.unitWidth, Direction.LTR, Side.Right)),
        TipSpecifications(Vertical, Vertical))),
      BlockedHorizontalConcatenation(HorizontalConcatenation(List(
          Space(2*Layout.unitWidth, Direction.LTR, Side.Left),
          Station("thing1b", true, Direction.LTR, FontInfo("monospace", "normal", "normal", "1em")),
          Space(2*Layout.unitWidth, Direction.LTR, Side.Right)),
        TipSpecifications(Vertical, Vertical))),
      Direction.LTR,
      Polarity.+,
      TipSpecifications(Vertical, Vertical))

  val thing2 = HorizontalConcatenation(List(
      Space(2*Layout.unitWidth, Direction.LTR, Side.Left),
      Station("thing2", false, Direction.LTR, FontInfo("sans-serif", "normal", "normal", "1em"))),
    TipSpecifications(Vertical, Logical(1)))
  val w = thing1.width

  val testLayout: Layout =
    HorizontalConcatenation(HorizontalConcatenation.adjustHeights(List(
      VerticalEpsilon(thing1, w + 3*Layout.unitWidth, Direction.LTR, Polarity.+, TipSpecifications(Logical(1), Logical(1))),
      thing2,
      VerticalEpsilon(thing1, w + thing2.width + 3*Layout.unitWidth, Direction.LTR, Polarity.+, TipSpecifications(Logical(2), Logical(1))),
      VerticalEpsilon(
        VerticalEpsilon(thing1, w + 5*Layout.unitWidth, Direction.LTR, Polarity.+, TipSpecifications(Logical(1), Logical(1))),
        w + 5*Layout.unitWidth, Direction.LTR, Polarity.-, TipSpecifications(Vertical, Logical(1))),
      thing2,
      VerticalEpsilon(thing1, w + thing2.width + 3*Layout.unitWidth, Direction.LTR, Polarity.-, TipSpecifications(Logical(1), Logical(1))),
      VerticalConcatenation(
        thing1,
        BlockedHorizontalConcatenation(HorizontalConcatenation(List(
            Space(2*Layout.unitWidth, Direction.LTR, Side.Left),
            Rail(66.51666, Direction.LTR),
            Space(2*Layout.unitWidth, Direction.LTR, Side.Right)),
          TipSpecifications(Vertical, Vertical))),
        Direction.LTR,
        Polarity.+,
        TipSpecifications(Vertical, Vertical)),
      LineBreak(100, Direction.LTR),
      thing2)),
      TipSpecifications(Logical(1), Logical(1)))

  val thing1rtl =
    VerticalConcatenation(
      BlockedHorizontalConcatenation(HorizontalConcatenation(List(
          Space(2*Layout.unitWidth, Direction.RTL, Side.Right),
          Station("thing1a", true, Direction.RTL, FontInfo("monospace", "normal", "normal", "1em")),
          Space(2*Layout.unitWidth, Direction.RTL, Side.Left)),
        TipSpecifications(Vertical, Vertical))),
      BlockedHorizontalConcatenation(HorizontalConcatenation(List(
          Space(2*Layout.unitWidth, Direction.RTL, Side.Right),
          Station("thing1b", true, Direction.RTL, FontInfo("monospace", "normal", "normal", "1em")),
          Space(2*Layout.unitWidth, Direction.RTL, Side.Left)),
        TipSpecifications(Vertical, Vertical))),
      Direction.RTL,
      Polarity.+,
      TipSpecifications(Vertical, Vertical))

  val thing2rtl = HorizontalConcatenation(List(
      Space(2*Layout.unitWidth, Direction.RTL, Side.Right),
      Station("thing2rtl", false, Direction.RTL, FontInfo("sans-serif", "normal", "normal", "1em"))),
    TipSpecifications(Logical(1), Vertical))
  val wrtl = thing1rtl.width

  val testLayoutrtl: Layout =
    HorizontalConcatenation(HorizontalConcatenation.adjustHeights(List(
      VerticalEpsilon(thing1rtl, wrtl + 3*Layout.unitWidth, Direction.RTL, Polarity.+, TipSpecifications(Logical(1), Logical(1))),
      thing2rtl,
      VerticalEpsilon(thing1rtl, wrtl + thing2rtl.width + 3*Layout.unitWidth, Direction.RTL, Polarity.+, TipSpecifications(Logical(1), Logical(2))),
      VerticalEpsilon(
        BlockedHorizontalConcatenation(HorizontalConcatenation(List(
            Space(2*Layout.unitWidth, Direction.RTL, Side.Right),
            Station("thing1", true, Direction.RTL, FontInfo("monospace", "normal", "normal", "1em")),
            Space(2*Layout.unitWidth, Direction.RTL, Side.Left)),
          TipSpecifications(Vertical, Vertical))),
        75.29999923706055, Direction.RTL, Polarity.-, TipSpecifications(Logical(1), Vertical)),
      thing2rtl,
      VerticalEpsilon(thing1rtl, wrtl + thing2rtl.width + 3*Layout.unitWidth, Direction.RTL, Polarity.-, TipSpecifications(Logical(1), Logical(1))),
      VerticalConcatenation(
        thing1rtl,
        BlockedHorizontalConcatenation(HorizontalConcatenation(List(
            Space(2*Layout.unitWidth, Direction.RTL, Side.Right),
            Rail(66.51666, Direction.RTL),
            Space(2*Layout.unitWidth, Direction.RTL, Side.Left)),
          TipSpecifications(Vertical, Vertical))),
        Direction.RTL,
        Polarity.+,
        TipSpecifications(Vertical, Vertical)),
      LineBreak(100, Direction.RTL),
      thing2rtl)),
      TipSpecifications(Logical(1), Logical(1)))


  @scalajs.js.annotation.JSExport
  def init(): Unit =
    document.addEventListener("DOMContentLoaded", (event) => {
      val svg = render(testLayout).render
      outputCanvas.appendChild(svg)
    })
