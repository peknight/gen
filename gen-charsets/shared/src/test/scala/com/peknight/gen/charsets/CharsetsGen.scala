package com.peknight.gen.charsets

import cats.data.StateT.pure
import cats.syntax.option.*
import cats.syntax.apply.*
import cats.syntax.either.*
import com.peknight.gen.charsets.GenCharsets.combineStartEnd
import cats.{Applicative, Monad}
import com.peknight.spire.ext.syntax.interval.close
import com.peknight.spire.ext.syntax.bound.{upper, lower}
import com.peknight.gen.GenT
import com.peknight.random.Random
import com.peknight.random.state.{nextBoolean, shuffle}
import spire.math.Interval
import spire.math.interval.*

object CharsetsGen:

  private[this] val chars: List[String] = List(
    (0 to 9).mkString,
    ('a' to 'z').mkString,
    ('A' to 'Z').mkString,
    "~!_@.#*$^&"
  )

  private[this] def charsetPointIntervalGen[F[_]: Applicative]: GenT[F, Interval[Int]] =
    GenT.choose(1, 33).map(Interval.point)

  private[this] def charsetAboveIntervalGen[F[_]: Monad]: GenT[F, Interval[Int]] =
    for
      close <- nextBoolean[F]
      lower <- GenT.choose(-16, 17)
    yield
      if close then Interval.atOrAbove(lower) else Interval.above(lower)

  private[this] def charsetBelowIntervalGen[F[_] : Monad]: GenT[F, Interval[Int]] =
    for
      close <- nextBoolean[F]
      upper <- GenT.choose(if close then 1 else 2, 33)
    yield
      if close then Interval.atOrBelow(upper) else Interval.below(upper)

  private[this] def charsetBoundedIntervalGen[F[_]: Monad]: GenT[F, Interval[Int]] =
    for
      closeLower <- nextBoolean[F]
      lower <- GenT.choose(-16, 17)
      closeUpper <- nextBoolean
      upperMin =
        if closeLower && closeUpper then (lower + 1) max 1
        else if closeUpper then (lower + 2) max 1
        else if closeLower then (lower + 2) max 2
        else (lower + 3) max 2
      upper <- GenT.choose(upperMin, (upperMin max 32) + 1)
    yield Interval.fromBounds(
      if closeLower then Closed(lower) else Open(lower),
      if closeUpper then Closed(upper) else Open(upper)
    )

  private[this] def charsetIntervalGen[F[_]: Monad]: GenT[F, Interval[Int]] = GenT.oneOf(
    pure(Interval.all[Int]),
    charsetPointIntervalGen,
    charsetAboveIntervalGen,
    charsetBelowIntervalGen,
    charsetBoundedIntervalGen
  )

  private[this] def charsetRepeatIntervalGen[F[_]: Monad](lowerLowerBound: Int, lowerUpperBound: Int)
  : GenT[F, Interval[Int]] =
    for
      closeLower <- nextBoolean[F]
      lower <- GenT.choose(
        if closeLower then lowerLowerBound else lowerLowerBound - 1,
        if closeLower then lowerUpperBound + 1 else lowerUpperBound
      )
      closeUpper <- nextBoolean
      upperMin =
        if closeUpper && closeLower then lower max 1
        else if closeUpper then (lower + 1) max 1
        else if closeLower then (lower + 1) max 2
        else (lower + 2) max 2
      upper <- GenT.choose(upperMin, (upperMin max 32) + 1)
    yield Interval.fromBounds(
      if closeLower then Closed(lower) else Open(lower),
      if closeUpper then Closed(upper) else Open(upper)
    )

  private[this] def charsetListGen[F[_]: Monad]: GenT[F, List[Charset[Iterable[Char]]]] =
    for
      size <- GenT.choose(1, chars.size + 1)
      chars <- shuffle[F, String, List[String]](chars)
      charsetList <- Monad[[A] =>> GenT[F, A]].tailRecM((
        List.empty[Charset[Iterable[Char]]], chars.take(size), false, false
      )) {
        case (acc, Nil, _, _) => pure(acc.reverse.asRight)
        case (acc, head :: tail, start, end) =>
          for
            repeat <- nextBoolean[F]
            startsWith <- if tail.isEmpty && !start then pure[F, Random[F], Boolean](true) else nextBoolean[F]
            endsWith <- if tail.isEmpty && !end then pure[F, Random[F], Boolean](true) else nextBoolean[F]
            lowerLowerBound = if tail.nonEmpty || (start && end) then 0 else if start || end then 1 else 2
            length <- if repeat then charsetIntervalGen else charsetRepeatIntervalGen(lowerLowerBound, chars.length)
          yield
            (Charset(head, length, repeat, startsWith, endsWith) :: acc, tail, start || startsWith, end || endsWith)
              .asLeft
      }
    yield
      charsetList

  private[this] def consecutiveGen[F[_]: Monad]: GenT[F, Consecutive] =
    (GenT.choose[F, Int](1, 9), GenT.choose[F, Int](0, 4), nextBoolean[F]).mapN(Consecutive.apply)

  def charsetsGen[F[_]: Monad]: GenT[F, Charsets[Iterable[Char]]] =
    for
      charsets <- charsetListGen
      elementLength = combineStartEnd(charsets.map { charset =>
        val length =
          if charset.repeat then charset.length.close & Interval.atOrAbove(0)
          else charset.length.close & Interval.atOrBelow(charset.chars.size)
        charset.copy(length = length)
      }.zipWithIndex.map(_.swap).toMap)
      lowerUpperBound = elementLength.upperBound match
        case upperBound: ValueBound[_] => upperBound.upper
        case _ => 64
      upperLowerBound = elementLength.lowerBound match
        case lowerBound: ValueBound[_] => lowerBound.lower
        case _ => 1
      closeLower <- nextBoolean
      lower <- GenT.choose(if closeLower then 1 else 0, if closeLower then lowerUpperBound + 1 else lowerUpperBound)
      closeUpper <- nextBoolean
      upperMin =
        if closeUpper && closeLower then upperLowerBound max lower
        else if closeUpper then upperLowerBound max (lower + 1)
        else if closeLower then (upperLowerBound + 1) max (lower + 1)
        else (upperLowerBound + 1) max (lower + 2)
      upper <- GenT.choose(upperMin, (upperMin max 128) + 1)
      consecutiveOption <- GenT.oneOf(pure(none[Consecutive]), consecutiveGen.map(_.some))
      retry <- pure(0)
    yield Charsets(charsets, Interval.fromBounds(
      if closeLower then Closed(lower) else Open(lower),
      if closeUpper then Closed(upper) else Open(upper)
    ), consecutiveOption, retry)

end CharsetsGen
