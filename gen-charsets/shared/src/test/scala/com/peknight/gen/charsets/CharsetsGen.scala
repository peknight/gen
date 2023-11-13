package com.peknight.gen.charsets

import cats.data.StateT.pure
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.{Applicative, Monad}
import com.peknight.gen.Gen
import com.peknight.gen.charsets.CharsetsOps.combineStartEnd
import com.peknight.random.Random
import com.peknight.random.state.{nextBoolean, shuffle}
import com.peknight.spire.ext.syntax.bound.{lower, upper}
import com.peknight.spire.ext.syntax.interval.close
import spire.math.Interval
import spire.math.interval.*

object CharsetsGen:

  private[this] val chars: List[String] = List(
    (0 to 9).mkString,
    ('a' to 'z').mkString,
    ('A' to 'Z').mkString,
    "~!_@.#*$^&"
  )

  private[this] def charsetPointIntervalGen[F[_]: Applicative]: Gen[F, Interval[Int]] =
    Gen.choose(1, 33).map(Interval.point)

  private[this] def charsetAboveIntervalGen[F[_]: Monad]: Gen[F, Interval[Int]] =
    for
      close <- nextBoolean[F]
      lower <- Gen.choose(-16, 17)
    yield
      if close then Interval.atOrAbove(lower) else Interval.above(lower)

  private[this] def charsetBelowIntervalGen[F[_] : Monad]: Gen[F, Interval[Int]] =
    for
      close <- nextBoolean[F]
      upper <- Gen.choose(if close then 1 else 2, 33)
    yield
      if close then Interval.atOrBelow(upper) else Interval.below(upper)

  private[this] def charsetBoundedIntervalGen[F[_]: Monad]: Gen[F, Interval[Int]] =
    for
      closeLower <- nextBoolean[F]
      lower <- Gen.choose(-16, 17)
      closeUpper <- nextBoolean
      upperMin =
        if closeLower && closeUpper then (lower + 1) max 1
        else if closeUpper then (lower + 2) max 1
        else if closeLower then (lower + 2) max 2
        else (lower + 3) max 2
      upper <- Gen.choose(upperMin, (upperMin max 32) + 1)
    yield Interval.fromBounds(
      if closeLower then Closed(lower) else Open(lower),
      if closeUpper then Closed(upper) else Open(upper)
    )

  private[this] def charsetIntervalGen[F[_]: Monad]: Gen[F, Interval[Int]] = Gen.oneOf(
    pure(Interval.all[Int]),
    charsetPointIntervalGen,
    charsetAboveIntervalGen,
    charsetBelowIntervalGen,
    charsetBoundedIntervalGen
  )

  private[this] def charsetBoundedIntervalGen[F[_]: Monad](lowerLowerBound: Int, lowerUpperBound: Int)
  : Gen[F, Interval[Int]] =
    for
      closeLower <- nextBoolean[F]
      lower <- Gen.choose(
        if closeLower then lowerLowerBound else lowerLowerBound - 1,
        if closeLower then lowerUpperBound + 1 else lowerUpperBound
      )
      closeUpper <- nextBoolean
      upperMin =
        if closeUpper && closeLower then lower max 1
        else if closeUpper then (lower + 1) max 1
        else if closeLower then (lower + 1) max 2
        else (lower + 2) max 2
      upper <- Gen.choose(upperMin, (upperMin max 32) + 1)
    yield Interval.fromBounds(
      if closeLower then Closed(lower) else Open(lower),
      if closeUpper then Closed(upper) else Open(upper)
    )

  private[this] def charsetListGen[F[_]: Monad]: Gen[F, List[Charset[Iterable[Char]]]] =
    for
      size <- Gen.choose(1, chars.size + 1)
      chars <- shuffle[F, String, List[String]](chars)
      charsetList <- Monad[[A] =>> Gen[F, A]].tailRecM((
        List.empty[Charset[Iterable[Char]]], chars.take(size), false, false
      )) {
        case (acc, Nil, _, _) => pure(acc.reverse.asRight)
        case (acc, head :: tail, start, end) =>
          for
            repeat <- nextBoolean[F]
            startsWith <- if tail.isEmpty && !start then pure[F, Random[F], Boolean](true) else nextBoolean[F]
            endsWith <- if tail.isEmpty && !end then pure[F, Random[F], Boolean](true) else nextBoolean[F]
            lowerLowerBound =
              if startsWith && endsWith then 2
              else if tail.nonEmpty || (start && end) then 0
              else if start || end then 1
              else 2
            length <-
              if repeat && lowerLowerBound == 0 then charsetIntervalGen
              else charsetBoundedIntervalGen(lowerLowerBound, chars.length)
          yield
            (Charset(head, length, repeat, startsWith, endsWith) :: acc, tail, start || startsWith, end || endsWith)
              .asLeft
      }
    yield
      charsetList

  private[this] def consecutiveGen[F[_]: Monad]: Gen[F, Consecutive] =
    (Gen.choose[F, Int](1, 9), Gen.choose[F, Int](0, 4), nextBoolean[F])
      .mapN(Consecutive.apply)

  def apply[F[_]: Monad]: Gen[F, Charsets[Iterable[Char]]] =
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
      lower <- Gen.choose(if closeLower then 1 else 0, if closeLower then lowerUpperBound + 1 else lowerUpperBound)
      closeUpper <- nextBoolean
      upperMin =
        if closeUpper && closeLower then upperLowerBound max lower
        else if closeUpper then upperLowerBound max (lower + 1)
        else if closeLower then (upperLowerBound + 1) max (lower + 1)
        else (upperLowerBound + 1) max (lower + 2)
      upper <- Gen.choose(upperMin, (upperMin max 128) + 1)
      consecutiveOption <- Gen.oneOf(pure(none[Consecutive]), consecutiveGen.map(_.some))
      retry <- pure(128)
    yield Charsets(charsets, Interval.fromBounds(
      if closeLower then Closed(lower) else Open(lower),
      if closeUpper then Closed(upper) else Open(upper)
    ), consecutiveOption, retry)

end CharsetsGen
