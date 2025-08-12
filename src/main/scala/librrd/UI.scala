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

  class Debouncer(private val action: () => Unit, val timeout: Int):
    private var timer: Option[Int] = None
    def trigger(): Unit =
      timer.foreach(dom.window.clearTimeout(_))
      timer = Some(dom.window.setTimeout(() => action(), timeout))

  class InputPresetState(val ip: InputsPresets, val onDone: () => Unit):
    private def presetState(suppressDone: Boolean = false): Unit =
      ip.presets.get(ip.preset.value).foreach{ ip.input.value = _ }
      if !suppressDone then onDone()

    private val debouncer = Debouncer(() => customDoneState(), 500)
    private def customTypingState(): Unit =
      debouncer.trigger()

    private def customDoneState(): Unit = onDone()

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

  class ResizeState():
    var width = 500.0
    private val debouncer = Debouncer(() => reLayOut(width), 10)

    def register(): Unit =
      dom.ResizeObserver{ (entries, o) =>
        entries.foreach{ entry =>
          if entry.target.id == "output" then
            val newHeight = entry.borderBoxSize(0).blockSize
            val newWidth = entry.borderBoxSize(0).inlineSize
            // default value of preserveAspectRatio takes care of AR
            outputCanvas.setAttribute("viewBox", s"-10 -10 $newWidth $newHeight")
            width = newWidth - 20
            debouncer.trigger()
        }
      }.observe(document.getElementById("output"))

  def registerInputs(): Unit =
    val resize = ResizeState()
    resize.register()
    InputPresetState(InputsPresets.Diagram, () => reLayOut(resize.width)).register(true)
    InputPresetState(InputsPresets.Layout, () => reLayOut(resize.width)).register(true)
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

  var oldSVG: Option[org.scalajs.dom.Node] = None
  def reLayOut(width: Double): Unit =
    val myParameterizedDiagram = ParameterizedDiagrams.parameterize(getDiagram, getStylesheet)
    val myDirectedDiagram = DirectedDiagrams.direct(myParameterizedDiagram)
    val myAlignedDiagram = AlignedDiagrams.align(myDirectedDiagram)
    val myWrappedDiagram = summon[WrappedDiagrams[Tag]].wrapLocally(myAlignedDiagram)
    val myLayout = JustifiedDiagrams.justify(myWrappedDiagram, width)
    val myRendering = summon[WrappedDiagrams[Tag]].backend.render(myLayout)
    val mySVG = myRendering.render
    if oldSVG.isDefined then
      outputCanvas.replaceChild(mySVG, oldSVG.get)
    else
      outputCanvas.appendChild(mySVG)
    oldSVG = Some(mySVG)

  lazy val customStyleElement = document.getElementById("custom-style")
  def reRender(): Unit =
    customStyleElement.innerHTML = InputsPresets.Rendering.input.value

  @main def main(): Unit =
    document.addEventListener("DOMContentLoaded", (event) => {
      registerInputs()
    })
