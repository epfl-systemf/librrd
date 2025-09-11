package ui

import org.scalajs.dom
import org.scalajs.dom.document
import librrd.LibRRD

object UI:
  lazy val outputCanvas = document.getElementById("output-canvas")

  class Debouncer(private val action: () => Unit, val timeout: Int):
    private var timer: Option[Int] = None
    def trigger(): Unit =
      timer.foreach(dom.window.clearTimeout(_))
      timer = Some(dom.window.setTimeout(() => action(), timeout))

  val customPreset = "custom…"
  enum InputsPresets[T](
      val inputID: String,
      val presetID: String,
      val presets: Map[String, String],
      val parser: InputParser[T],
      val onDone: () => Unit):
    val input = document.getElementById(inputID).asInstanceOf[dom.HTMLTextAreaElement]
    val preset = document.getElementById(presetID).asInstanceOf[dom.HTMLSelectElement]

    case Diagram extends InputsPresets(
      "diagram-input",
      "diagram-preset",
      UIPresets.diagramPresets,
      DiagramParser,
      reLayOutAndSyncPresets)

    case LayoutStylesheet extends InputsPresets(
      "layout-input",
      "layout-preset",
      UIPresets.layoutPresets,
      StylesheetParser,
      reLayOut)

    case RenderingStylesheet extends InputsPresets(
      "rendering-input",
      "rendering-preset",
      UIPresets.renderingPresets,
      IdentityParser,
      reRender)

    private def presetState(suppressDone: Boolean = false): Unit =
      if preset.value != customPreset then
        input.value = presets.get(preset.value).get
        input.setCustomValidity("")
        input.reportValidity(): Unit
      if !suppressDone then onDone()

    private val debouncer = Debouncer(() => customDoneState(), 500)
    private def customTypingState(): Unit =
      parser(input.value) match
        case util.Success(_) =>
          debouncer.trigger()
          input.setCustomValidity("")
        case util.Failure(exception) =>
          input.setCustomValidity(exception.getMessage())
      input.reportValidity(): Unit

    private def customDoneState(): Unit = onDone()

    def get: T = parser(input.value).get

    def register(suppressInitialDone: Boolean = false): Unit =
      (presets.keys.toVector.sorted :+ customPreset).map{ key =>
          val opt = document.createElement("option")
          opt.innerText = key
          opt
        }.foreach(preset.appendChild)
      presetState(suppressInitialDone)
      preset.addEventListener("change", (event) => {
        if preset.value == customPreset then
          input.value = ""
          customDoneState()
        else
          presetState()
      })
      input.addEventListener("input", (event) => {
        preset.value = customPreset
        customTypingState()
      })

  lazy val outputDiv = document.getElementById("output")

  object ResizeState:
    var width = 500.0
    private val debouncer = Debouncer(reLayOut, 10)

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
      }.observe(outputDiv)

  lazy val saveOutputButton = document.getElementById("save-output")
  lazy val helpDialog = document.getElementById("help-dialog").asInstanceOf[dom.HTMLDialogElement]
  def registerInputs(): Unit =
    ResizeState.register()
    InputsPresets.Diagram.register(true)
    InputsPresets.LayoutStylesheet.register(true)
    InputsPresets.RenderingStylesheet.register()
    saveOutputButton.addEventListener("click", (event) => {
      val serialized = dom.XMLSerializer().serializeToString(outputCanvas)
      val blob = dom.Blob(scalajs.js.Array(serialized),
        new dom.BlobPropertyBag { `type` = "image/svg+xml" })
      val downloader = document.createElement("a")
      downloader.setAttribute("href", dom.URL.createObjectURL(blob))
      downloader.setAttribute("download", "librrd.svg")
      downloader.setAttribute("style", "display: none;")
      outputDiv.appendChild(downloader)
      downloader.dispatchEvent(dom.PointerEvent("click"))
      outputDiv.removeChild(downloader)
    })
    document.getElementById("help-button").addEventListener("click", (event) => {
      helpDialog.showModal()
    })

  def reLayOutAndSyncPresets(): Unit =
    val firstWordIdx = InputsPresets.Diagram.preset.value.indexOf(' ')
    if firstWordIdx >= 0 then
      val style = InputsPresets.Diagram.preset.value.substring(0, firstWordIdx)
      def syncPreset[T](ip: InputsPresets[T]) =
        if ip.presets.contains(style) && ip.preset.value != customPreset then
          ip.preset.value = style
          ip.preset.dispatchEvent(dom.InputEvent("change")): Unit
      syncPreset(InputsPresets.LayoutStylesheet)
      syncPreset(InputsPresets.RenderingStylesheet)
    reLayOut()

  var oldSVG: Option[org.scalajs.dom.Node] = None
  def reLayOut(): Unit =
    LibRRD.resetSVGID()
    val mySVG = LibRRD.layOutToSVG(
      InputsPresets.Diagram.get,
      InputsPresets.LayoutStylesheet.get,
      ResizeState.width).render
    if oldSVG.isDefined then
      outputCanvas.replaceChild(mySVG, oldSVG.get)
    else
      outputCanvas.appendChild(mySVG)
    oldSVG = Some(mySVG)

  lazy val outputStyleElement = document.getElementById("output-style")
  def reRender(): Unit =
    outputStyleElement.innerHTML = InputsPresets.RenderingStylesheet.get

  @main def main(): Unit =
    document.addEventListener("DOMContentLoaded", (event) => {
      registerInputs()
    })
