package ui

import scalajs.js
import js.annotation.*
import org.rogach.scallop.*
import GUIPresets.*
import librrd.LayoutsSVGFile

object CLI:

  @js.native
  @JSImport("process", "argv")
  val argv: js.Array[String] = js.native

  @js.native
  @JSImport("fs", "existsSync")
  def existsSync(path: String): Boolean = js.native

  trait ReadOptions extends js.Object:
    val encoding: String
  val ReadUTF8 = new ReadOptions { val encoding = "utf8" }
  @js.native
  @JSImport("fs", "readFileSync")
  def readFileSync(path: String, options: ReadOptions): String = js.native

  def firstWord(str: String) =
    val idx = str.indexOf(' ')
    if idx > 0 then Some(str.substring(0, idx)) else None

  def withSuffixSVG(path: String) =
    val lastSlashIdx = Math.max(path.lastIndexOf('/'), 0)
    val suffixIdx = path.indexOf('.', lastSlashIdx)
    (if suffixIdx > 0 then path.substring(0, suffixIdx) else path) + ".svg"

  class Config(args: Seq[String]) extends ScallopConf(args):
    helpWidth(100)
    val layoutStylesheet = opt[String](
      name = "layout-stylesheet",
      descr = "path to layout stylesheet file, OR layout stylesheet preset name",
      validate = (d => layoutPresets.contains(d) || existsSync(d)),
    )
    val layoutStylesheetAppend = opt[String](
      name = "layout-stylesheet-append",
      short = 'L',
      descr = "path to layout stylesheet file to append to existing stylesheet",
      validate = existsSync,
    )
    val renderingStylesheet = opt[String](
      name = "rendering-stylesheet",
      descr = "path to rendering stylesheet file, OR rendering stylesheet preset name",
      validate = (d => renderingPresets.contains(d) || existsSync(d)),
    )
    val renderingStylesheetAppend = opt[String](
      name = "rendering-stylesheet-append",
      short = 'R',
      descr = "path to rendering stylesheet file to append to existing stylesheet",
      validate = existsSync,
    )
    val output = opt[String](name = "output", descr = "path to output file")
    val time = opt[Boolean](
      name = "time",
      descr = "whether to measure and report elapsed time for layout"
    )
    val globalWrapping = opt[Boolean](
      name = "global-wrapping",
      descr = "whether to wrap globally"
    )
    val renderOnly = opt[Boolean](
      name = "render-only",
      descr = "do not perform layout; treat diagram input as a layout, and just render it, " +
        "ignoring all other options except rendering-stylesheet"
    )
    val diagram = trailArg[String](
      name = "diagram",
      descr = "path to diagram file, OR diagram preset name",
      validate = (d => diagramPresets.contains(d) || existsSync(d)),
    )
    val width = trailArg[Double](
      name = "width",
      descr = "target width for layout; values between 0 and 1 are interpreted as proportions of max-content",
      validate = (w => w > 0),
    )
    verify()

  def main =
    val config = Config(argv.toSeq.drop(2))
    val diagram =
      if existsSync(config.diagram()) then readFileSync(config.diagram(), ReadUTF8)
      else diagramPresets(config.diagram())
    val renderingStylesheet =
      (if config.renderingStylesheet.isEmpty
       then firstWord(config.diagram())
         .flatMap(renderingPresets.get)
         .getOrElse(renderingPresets("default"))
       else if existsSync(config.renderingStylesheet())
       then readFileSync(config.renderingStylesheet(), ReadUTF8)
       else renderingPresets(config.renderingStylesheet()))
      + config.renderingStylesheetAppend.map(readFileSync(_, ReadUTF8)).getOrElse("")
    val outputFilename = config.output.getOrElse(withSuffixSVG(config.diagram()))
    if config.renderOnly() then
      val myLayoutParser = LayoutParser(LayoutsSVGFile)
      val myLayout = myLayoutParser(diagram).get
      LayoutsSVGFile.renderToFile(myLayoutParser.backend.render(myLayout),
        renderingStylesheet, outputFilename)
    else
      val layoutStylesheet =
        (if config.layoutStylesheet.isEmpty
         then firstWord(config.diagram())
           .flatMap(layoutPresets.get)
           .getOrElse(layoutPresets("default"))
         else if existsSync(config.layoutStylesheet())
         then readFileSync(config.layoutStylesheet(), ReadUTF8)
         else layoutPresets(config.layoutStylesheet()))
        + config.layoutStylesheetAppend.map(readFileSync(_, ReadUTF8)).getOrElse("")
      (if config.globalWrapping()
       then librrd.LibRRDFile.layOutGloballyToSVGFile
       else librrd.LibRRDFile.layOutToSVGFile)(
        DiagramParser(diagram).get,
        StylesheetParser(layoutStylesheet).get,
        IdentityParser(renderingStylesheet).get,
        config.width(),
        outputFilename,
        config.time())
