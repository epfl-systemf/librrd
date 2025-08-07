package librrd

import LayoutStylesheets.PropertyMap

object JustifiedDiagrams:
  def justify[T](using wd: WrappedDiagrams[T])(diagram: wd.WrappedDiagram, targetWidth: Double)
      : wd.backend.Layout =
    import wd.backend as l
    def rec(diagram: wd.WrappedDiagram | wd.GlobalWrap
                     | wd.SequenceWrap[wd.GlobalWrap | wd.LocallyWrappedDiagram],
            targetWidth: Double): l.Layout =
      diagram match
        case wd.Station(label, isTerminal, direction, properties, classes, id) =>
          l.Station(label, isTerminal, direction, classes, id)
        case wd.Space(direction) =>
          l.Space(diagram.minContent, direction)
        case wd.BlockVerticalConcatenation(topSubdiagram, bottomSubdiagram,
            direction, polarity, properties, tipSpecs, classes, id) =>
          ???
        case gwd @ wd.GloballyWrappedDiagram(direction, properties, options) =>
          rec(gwd.bestUnder(targetWidth), targetWidth)
        case gsw @ wd.GlobalSequenceWrap(sw) => rec(sw, targetWidth)
        case lws @ wd.LocallyWrappedSequence(subdiagrams, direction, properties, tipSpecs, classes, id) =>
          rec(lws.bestUnder(targetWidth), targetWidth)
        case wd.HorizontalConcatenation(subs, direction, properties, classes, id) =>
          val subdiagrams = subs.toVector
          val n = subdiagrams.length
          var absorbed = (n-1)*MIN_GAP
          val distributed = subdiagrams.map(_.minContent).toArray
          var remaining = targetWidth - absorbed - distributed.sum

          val growth = subdiagrams.map(s => s.maxContent - s.minContent)
          val growthSum = growth.sum
          val maxGrowth = Math.min(remaining, growthSum)
          distributed.indices.foreach { i => distributed(i) += maxGrowth * growth(i)/growthSum }
          remaining -= maxGrowth

          val flexAbsorbed = properties.get(LayoutStylesheets.FlexAbsorb) * remaining
          absorbed += flexAbsorbed
          remaining -= flexAbsorbed

          val concatenations = subdiagrams.map(s =>
            if (s.isInstanceOf[l.Station] || s.isInstanceOf[l.Space]) then None else Some(s))
          if concatenations.forall(_.isEmpty)
          then
            absorbed += remaining
          else
            val concatMaxs = concatenations.map(_.map(_.maxContent))
            val concatMaxsSum = concatMaxs.map(_.getOrElse(0.0)).sum
            distributed.indices.filter(i => concatenations(i).isDefined).foreach { i =>
              distributed(i) += remaining * concatMaxs(i).get / concatMaxsSum
            }
          remaining = 0
          ???
        case ivc @ wd.InlineVerticalConcatenation[(wd.LocallyWrappedDiagram | wd.GlobalWrap)]
            (subdiagrams, direction, properties, tipSpecs, classes, id) =>
          l.InlineVerticalConcatenation(subdiagrams.map(s => rec(s, targetWidth - ivc.extraWidth)),
            properties.get(LayoutStylesheets.ContinuationMarker), tipSpecs, classes, id)
    rec(diagram, targetWidth)
