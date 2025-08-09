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
        case wd.Station(label, isTerminal, direction, properties, numRows, classes, id) =>
          l.Station(label, isTerminal, direction, classes, id)
        case wd.Space(direction, numRows) =>
          l.Space(diagram.minContent, direction)
        case bvc @ wd.BlockVerticalConcatenation(topSubdiagram, bottomSubdiagram,
            direction, polarity, properties, tipSpecs, numRows, classes, id) =>
          val width = targetWidth - bvc.extraWidth
          l.BlockVerticalConcatenation(rec(topSubdiagram, width), rec(bottomSubdiagram, width),
            direction, polarity, tipSpecs, numRows, bvc.extraWidths, classes, id)
        case ivc @ wd.InlineVerticalConcatenation[(wd.LocallyWrappedDiagram | wd.GlobalWrap)]
            (subdiagrams, direction, properties, tipSpecs, numRows, classes, id) =>
          val width = targetWidth - ivc.extraWidth
          l.InlineVerticalConcatenation(
            ivc.firsts.map(s => rec(s, width - ivc.markerWidth))
            ++ ivc.mids.map(s => rec(s, width - 2*ivc.markerWidth))
            ++ ivc.lasts.map(s => rec(s, width - ivc.markerWidth)),
            properties.get(LayoutStylesheets.ContinuationMarker), tipSpecs, numRows, ivc.extraWidths,
            classes, id)
        case gwd @ wd.GloballyWrappedDiagram(direction, properties, numRows, options) =>
          rec(gwd.bestUnder(targetWidth), targetWidth)
        case gsw @ wd.GlobalSequenceWrap(sw) => rec(sw, targetWidth)
        case lws @ wd.LocallyWrappedSequence(
            subdiagrams, direction, properties, tipSpecs, numRows, classes, id) =>
          rec(lws.bestUnder(targetWidth), targetWidth)

        case wd.HorizontalConcatenation[(wd.LocallyWrappedDiagram | wd.GlobalWrap)]
            (subs, direction, properties, numRows, classes, id) =>
          val maybeSpaces = SidedProperty(subs.head, subs.last).map(_ match
            case sp: wd.Space => Some(rec(sp, sp.minContent))
            case _ => None)
          val subdiagrams = subs.toVector
            .drop(if maybeSpaces.left.isDefined then 1 else 0)
            .dropRight(if maybeSpaces.right.isDefined then 1 else 0)
          val n = subdiagrams.length
          var absorbed = Math.max(0, (n-1)*MIN_GAP)
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
          assert(absorbed + distributed.sum == targetWidth, "justification implementation error")

          val justificationRails = properties.get(LayoutStylesheets.JustifyContent)
            .distribute(absorbed, n, direction)
            .map(w => l.Rail(w, direction))
          val justifiedSubdiagrams = subdiagrams.zip(distributed).map(rec.tupled)
          l.HorizontalConcatenation(
            maybeSpaces.left.toList
            ++ (justificationRails.head // one more than n
                 +: justifiedSubdiagrams.zip(justificationRails).flatMap[l.Layout](_.toList))
            ++ maybeSpaces.right.toList,
            classes, id)

    rec(diagram, targetWidth)
