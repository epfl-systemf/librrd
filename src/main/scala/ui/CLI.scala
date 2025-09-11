package ui

import scalajs.js
import js.annotation.*
import org.rogach.scallop.*
import GUIPresets.*

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
    val layoutStylesheet = opt[String](
      name = "layout-stylesheet",
      descr = "path to layout stylesheet file, OR layout stylesheet preset name",
      validate = (d => layoutPresets.contains(d) || existsSync(d)),
    )
    val renderingStylesheet = opt[String](
      name = "rendering-stylesheet",
      descr = "path to rendering stylesheet file, OR rendering stylesheet preset name",
      validate = (d => renderingPresets.contains(d) || existsSync(d)),
    )
    val output = opt[String](name = "output", descr = "path to output file")
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
    val layoutStylesheet =
      if config.layoutStylesheet.isEmpty
      then firstWord(config.diagram())
        .flatMap(layoutPresets.get)
        .getOrElse(layoutPresets("default"))
      else if existsSync(config.layoutStylesheet())
      then readFileSync(config.layoutStylesheet(), ReadUTF8)
      else layoutPresets(config.layoutStylesheet())
    val renderingStylesheet =
      if config.renderingStylesheet.isEmpty
      then firstWord(config.diagram())
        .flatMap(renderingPresets.get)
        .getOrElse(renderingPresets("default"))
      else if existsSync(config.renderingStylesheet())
      then readFileSync(config.renderingStylesheet(), ReadUTF8)
      else renderingPresets(config.renderingStylesheet())
    librrd.LibRRDFile.layOutToSVGFile(
      DiagramParser(diagram).get,
      StylesheetParser(layoutStylesheet).get,
      IdentityParser(renderingStylesheet).get,
      config.width(),
      config.output.getOrElse(withSuffixSVG(config.diagram())))
