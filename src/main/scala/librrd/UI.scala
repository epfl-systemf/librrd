package librrd

import org.scalajs.dom
import org.scalajs.dom.document
import scalatags.JsDom.Tag

object UI:
  lazy val outputCanvas = document.getElementById("output-canvas")

  val customPreset = "custom…"
  enum InputsPresets(val inputID: String, val presetID: String, val presets: Map[String, String]):
    val input = document.getElementById(inputID).asInstanceOf[dom.HTMLTextAreaElement]
    val preset = document.getElementById(presetID).asInstanceOf[dom.HTMLSelectElement]
    case Diagram extends InputsPresets(
      "diagram-input",
      "diagram-preset",
      Map("JSON list" -> "(\"[\" (+ () (- \"[token]\" \",\")) \"]\")"))
    case Layout extends InputsPresets(
      "layout-input",
      "layout-preset",
      Map("JSON" -> "foo"))
    case Rendering extends InputsPresets(
      "rendering-input",
      "rendering-preset",
      Map(
        "JSON" -> "foo",
        "default" -> "@import url(librrd-default.css);"))

  class InputPresetState(val ip: InputsPresets, val onDone: () => Unit):
    def presetState(suppressDone: Boolean = false): Unit =
      ip.presets.get(ip.preset.value).foreach{ ip.input.value = _ }
      if !suppressDone then onDone()

    var timer: Option[Int] = None
    def customTypingState(): Unit =
      timer.foreach(dom.window.clearTimeout(_))
      timer = Some(dom.window.setTimeout(() => customDoneState(), 500))

    def customDoneState(): Unit = onDone()

    def register(suppressInitialDone: Boolean = false): Unit =
      presetState(suppressInitialDone)
      ip.preset.addEventListener("change", (event) => {
        if ip.preset.value == customPreset then
          ip.input.value = ""
          customDoneState()
        else
          presetState()
      })
      ip.input.addEventListener("input", (event) => {
        ip.preset.value = customPreset
        customTypingState()
      })

  def registerInputsPresets(): Unit =
    InputPresetState(InputsPresets.Diagram, reLayOut).register(true)
    InputPresetState(InputsPresets.Layout, reLayOut).register()
    InputPresetState(InputsPresets.Rendering, reRender).register()

  given WrappedDiagrams[Tag] = WrappedDiagrams(LayoutsSVG)

  def getStylesheet: LayoutStylesheets.Stylesheet =
    LayoutStylesheets.Stylesheet(Seq())

  def getDiagram: Diagrams.Diagram =
    Diagrams.Sequence(Seq(
      Diagrams.Stack(
        Diagrams.TerminalToken("railroad"),
        Diagrams.TerminalToken("syntax"),
        Polarity.+),
      Diagrams.NonterminalToken("diagram")))

  def reLayOut(): Unit =
    val myParameterizedDiagram = ParameterizedDiagrams.parameterize(getDiagram, getStylesheet)
    val myDirectedDiagram = DirectedDiagrams.direct(myParameterizedDiagram)
    val myAlignedDiagram = AlignedDiagrams.align(myDirectedDiagram)
    val myWrappedDiagram = summon[WrappedDiagrams[Tag]].wrapLocally(myAlignedDiagram)
    val myLayout = JustifiedDiagrams.justify(myWrappedDiagram, 500)
    val myRendering = summon[WrappedDiagrams[Tag]].backend.render(myLayout)
    outputCanvas.appendChild(myRendering.render)
    println("relaidout")

  lazy val customStyleElement = document.getElementById("custom-style")
  def reRender(): Unit =
    customStyleElement.innerHTML = InputsPresets.Rendering.input.value
    println("rerendered")

  def registerOutputResize(): Unit =
    dom.ResizeObserver{ (entries, o) =>
      entries.foreach{ entry =>
        if entry.target.id == "output" then
          val height = entry.borderBoxSize(0).blockSize
          val width = entry.borderBoxSize(0).inlineSize
          val ypad = 10.0 * height/width
          entry.target.setAttribute("viewbox", s"-10 -$ypad ${width + 10} ${height + ypad}")
      }
    }.observe(document.getElementById("output"))

  @main def main(): Unit =
    document.addEventListener("DOMContentLoaded", (event) => {
      registerInputsPresets()
      registerOutputResize()
    })
