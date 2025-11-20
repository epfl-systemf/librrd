package librrd

object JustifiedDiagrams:
  def justify[T](wd: WrappedDiagrams[T])(diagram: wd.WrappedDiagram, targetWidth: Double)
      : wd.backend.Layout =
    import wd.backend as l
    def rec(diagram: wd.ContentWidthFields,
            targetWidth: Double,
            depth: Int): l.Layout =
      val depthRec = (d, w) => rec(d, w, depth + 1)
      diagram match
        case wd.Station(label, isTerminal, direction, properties, numRows, font, classes, id) =>
          l.Station(label, isTerminal, direction, font, classes, id)
        case wd.Space(direction, numRows, verticalSide) =>
          l.Space(diagram.minContent, direction, verticalSide)
        case bvc @ wd.BlockVerticalConcatenation(topSubdiagram, bottomSubdiagram,
            direction, polarity, properties, tipSpecs, numRows, classes, id) =>
          val width = targetWidth - bvc.extraWidth
          val vc = l.VerticalConcatenation(
            depthRec(topSubdiagram, width).block,
            depthRec(bottomSubdiagram, width).block,
            direction, polarity, tipSpecs, classes, id)
          assert(numRows == vc.tipRows)
          // assert(bvc.extraWidths == 0)
          vc
        case ivc: wd.InlineVerticalConcatenation[_] =>
          val width = targetWidth - ivc.extraWidth
          val subs = depthRec(ivc.first, width - ivc.markerWidth)
            +: ivc.mids.map(s => depthRec(s, width - 2*ivc.markerWidth))
            :+ depthRec(ivc.last, width - ivc.markerWidth)
          val subsWithBreaks = subs
            .zipAll(List(), l.Rail(0, ivc.direction), l.LineBreak(width - 2*ivc.markerWidth, ivc.direction))
            .flatMap(ss => List(ss._1, ss._2))
            .dropRight(1)
          l.BlockedHorizontalConcatenation(l.HorizontalConcatenation(
            l.HorizontalConcatenation.adjustHeights(subsWithBreaks),
            ivc.classes, ivc.id),
            Some(ivc.tipSpecs))
        case gwd: wd.GloballyWrappedDiagram =>
          depthRec(gwd.bestUnder(targetWidth, depth), targetWidth)
        case gsw @ wd.GlobalSequenceWrap(sw, _) => depthRec(sw, targetWidth)
        case lws: wd.LocallyWrappedSequence[_] =>
          depthRec(lws.bestUnder(targetWidth, depth), targetWidth)

        case hc: wd.HorizontalConcatenation[_] =>
          val subdiagrams = hc.subdiagrams.toVector
          val n = subdiagrams.count(!_.isInstanceOf[wd.Space])
          var absorbed = Math.max(0, (n-1)*hc.properties.get(LayoutStylesheets.Gap))
          val distributed = subdiagrams.map(_.minContent).toArray
          var remaining = targetWidth - absorbed - distributed.sum

          val growth = subdiagrams.map(s => s.maxContent - s.minContent)
          val growthSum = growth.sum
          val maxGrowth = Math.min(remaining, growthSum)
          if growthSum > 0 then
            distributed.indices.foreach { i => distributed(i) += maxGrowth * growth(i)/growthSum }
            remaining -= maxGrowth

          val centered = hc.properties.get(LayoutStylesheets.JustifyContent) match
              case JustifyContentPolicy.Center | JustifyContentPolicy.SpaceAround
                | JustifyContentPolicy.SpaceBetween | JustifyContentPolicy.SpaceEvenly => true
              case _ => false
          val flexAbsorbed = (if n == 1 && centered then 0 else hc.properties.get(LayoutStylesheets.FlexAbsorb)) * remaining
          absorbed += flexAbsorbed
          remaining -= flexAbsorbed

          val concatenations = subdiagrams.map(s =>
            if (s.isInstanceOf[wd.Station] || s.isInstanceOf[wd.Space]) then None else Some(s))
          val concatMaxs = concatenations.map(_.map(_.maxContent))
          val concatMaxsSum = concatMaxs.map(_.getOrElse(0.0)).sum
          if concatMaxsSum == 0
          then
            absorbed += remaining
          else
            distributed.indices.filter(i => concatenations(i).isDefined).foreach { i =>
              distributed(i) += remaining * concatMaxs(i).get / concatMaxsSum
            }
          remaining = 0
          assert(absorbed + distributed.sum ~= targetWidth,
            "justification implementation error")

          val justificationRails = hc.properties.get(LayoutStylesheets.JustifyContent)
            .distribute(absorbed, n, hc.direction, hc.properties.get(LayoutStylesheets.Gap))
            .map(w => l.Rail(w, hc.direction))
          val (maybeSpaces, justifiedSubdiagrams) =
            trimSides(subdiagrams.zip(distributed).map(depthRec.tupled), { case sp: l.Space => sp })
          val sublayouts =
            maybeSpaces.left.toList
            ++ (justificationRails.head // one more than n
                +: justifiedSubdiagrams.zip(justificationRails.tail).flatMap[l.Layout](_.toList))
               .filterNot(_.width == 0)
            ++ maybeSpaces.right.toList
          if sublayouts.isEmpty then l.Rail(0, hc.direction, hc.classes, hc.id)
          else l.HorizontalConcatenation(hc.direction.reverse(sublayouts), hc.classes, hc.id)

    rec(diagram, targetWidth, 0)
