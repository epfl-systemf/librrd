package librrd

object JustifiedDiagrams:
  def justify[T](wd: WrappedDiagrams[T])(diagram: wd.WrappedDiagram, targetWidth: Double)
      : wd.backend.Layout =
    import wd.backend as l
    def rec(diagram: wd.WrappedDiagram | wd.GlobalWrap
                     | wd.SequenceWrap[wd.GlobalWrap | wd.LocallyWrappedDiagram],
            targetWidth: Double,
            depth: Int): l.Layout =
      val depthRec = (d, w) => rec(d, w, depth + 1)
      diagram match
        case wd.Station(label, isTerminal, direction, properties, numRows, font, classes, id) =>
          l.Station(label, isTerminal, direction, font, classes, id)
        case wd.Space(direction, numRows) =>
          l.Space(diagram.minContent, direction)
        case bvc @ wd.BlockVerticalConcatenation(topSubdiagram, bottomSubdiagram,
            direction, polarity, properties, tipSpecs, numRows, classes, id) =>
          val width = targetWidth - bvc.extraWidth
          l.BlockVerticalConcatenation(
            depthRec(topSubdiagram, width),
            depthRec(bottomSubdiagram, width),
            direction, polarity, tipSpecs, numRows, bvc.extraWidths, classes, id)
        case ivc @ wd.InlineVerticalConcatenation[(wd.LocallyWrappedDiagram | wd.GlobalWrap)]
            (subdiagrams, direction, properties, tipSpecs, numRows, classes, id) =>
          val width = targetWidth - ivc.extraWidth
          l.InlineVerticalConcatenation(
            depthRec(ivc.first, width - ivc.markerWidth)
            +: ivc.mids.map(s => depthRec(s, width - 2*ivc.markerWidth))
            :+ depthRec(ivc.last, width - ivc.markerWidth),
            properties.get(LayoutStylesheets.ContinuationMarker), tipSpecs, numRows, ivc.extraWidths,
            ivc.markerFont, classes, id)
        case gwd @ wd.GloballyWrappedDiagram(direction, properties, numRows, options) =>
          depthRec(gwd.bestUnder(targetWidth, depth), targetWidth)
        case gsw @ wd.GlobalSequenceWrap(sw) => depthRec(sw, targetWidth)
        case lws: wd.LocallyWrappedSequence =>
          depthRec(lws.bestUnder(targetWidth, depth), targetWidth)

        case hc @ wd.HorizontalConcatenation[(wd.LocallyWrappedDiagram | wd.GlobalWrap)]
            (subs, direction, properties, _, classes, id) =>
          val subdiagrams = subs.toVector
          val n = subdiagrams.count(!_.isInstanceOf[wd.Space])
          var absorbed = Math.max(0, (n-1)*MIN_GAP)
          val distributed = subdiagrams.map(_.minContent).toArray
          var remaining = targetWidth - absorbed - distributed.sum

          val growth = subdiagrams.map(s => s.maxContent - s.minContent)
          val growthSum = growth.sum
          val maxGrowth = Math.min(remaining, growthSum)
          if growthSum > 0 then
            distributed.indices.foreach { i => distributed(i) += maxGrowth * growth(i)/growthSum }
            remaining -= maxGrowth

          val flexAbsorbed = properties.get(LayoutStylesheets.FlexAbsorb) * remaining
          absorbed += flexAbsorbed
          remaining -= flexAbsorbed

          val concatenations = subdiagrams.map(s =>
            if (s.isInstanceOf[wd.Station] || s.isInstanceOf[wd.Space]) then None else Some(s))
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
          assert(absorbed + distributed.sum ~= targetWidth,
            "justification implementation error")

          val justificationRails = properties.get(LayoutStylesheets.JustifyContent)
            .distribute(absorbed, n, direction)
            .map(w => l.Rail(w, direction))
          val (maybeSpaces, justifiedSubdiagrams) =
            trimSides(subdiagrams.zip(distributed).map(depthRec.tupled), { case sp: l.Space => sp })
          l.HorizontalConcatenation(
            maybeSpaces.left.toList
            ++ (justificationRails.head // one more than n
                +: justifiedSubdiagrams.zip(justificationRails.tail).flatMap[l.Layout](_.toList))
               .filterNot(_.width == 0)
            ++ maybeSpaces.right.toList,
            hc.numRows, classes, id)

    rec(diagram, targetWidth, 0)
