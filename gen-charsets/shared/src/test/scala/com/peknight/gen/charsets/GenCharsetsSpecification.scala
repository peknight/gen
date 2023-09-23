package com.peknight.gen.charsets

import cats.Id
import cats.syntax.option.*
import cats.syntax.traverse.*
import com.peknight.cats.instances.scalacheck.gen.given
import com.peknight.gen.charsets.CharsetsGen.combineAll
import com.peknight.random.Random
import com.peknight.random.id.Random as IdRandom
import com.peknight.spire.ext.syntax.bound.{lower, upper}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}
import spire.math.Interval
import spire.math.interval.*

class GenCharsetsSpecification extends Properties("CharsetsGen"):

  val chars: List[String] = List((0 to 9).mkString, ('a' to 'z').mkString, ('A' to 'Z').mkString, "~!_@.#*$^&")
  val booleanGen: Gen[Boolean] = Gen.oneOf(true, false)
  val allIntervalGen: Gen[Interval[Int]] = Gen.const[Interval[Int]](Interval.all[Int])
  val charsetPointIntervalGen: Gen[Interval[Int]] = Gen.choose(1, 32).map(Interval.point)
  val charsetAboveIntervalGen: Gen[Interval[Int]] =
    for
      close <- booleanGen
      lower <- Gen.choose(-16, 16)
    yield if close then Interval.atOrAbove(lower) else Interval.above(lower)
  val charsetBelowIntervalGen: Gen[Interval[Int]] =
    for
      close <- booleanGen
      upper <- Gen.choose(1, 32)
    yield if close then Interval.atOrBelow(upper) else Interval.below(upper)
  val charsetBoundedIntervalGen: Gen[Interval[Int]] =
    for
      closeLower <- booleanGen
      lower <- Gen.choose(-16, 16)
      closeUpper <- booleanGen
      upperMin =
        if closeLower && closeUpper then (lower + 1) max 1
        else if closeUpper then (lower + 2) max 1
        else if closeLower then (lower + 2) max 2
        else (lower + 3) max 2
      upper <- Gen.choose(upperMin, upperMin max 32)
    yield Interval.fromBounds(
      if closeLower then Closed(lower) else Open(lower),
      if closeUpper then Closed(upper) else Open(upper)
    )
  def charsetsRepeatIntervalGen(length: Int): Gen[Interval[Int]] =
    for
      closeLower <- booleanGen
      lower <- Gen.choose(-16, if closeLower then length else length - 1)
      closeUpper <- booleanGen
      upperMin =
        if closeUpper && closeLower then 1 max lower
        else if closeUpper then (lower + 1) max 1
        else if closeLower then (lower + 1) max 2
        else (lower + 2) max 2
      upper <- Gen.choose(upperMin, upperMin max 32)
    yield Interval.fromBounds(
      if closeLower then Closed(lower) else Open(lower),
      if closeUpper then Closed(upper) else Open(upper)
    )

  val charsetIntervalGen: Gen[Interval[Int]] = Gen.oneOf(
    allIntervalGen,
    charsetPointIntervalGen,
    charsetAboveIntervalGen,
    charsetBelowIntervalGen,
    charsetBoundedIntervalGen
  )

  def charsetGen(chars: String): Gen[Charset[Iterable[Char]]] =
    for
      repeat <- booleanGen
      length <- if repeat then charsetIntervalGen else charsetsRepeatIntervalGen(chars.length)
      startsWith <- booleanGen
      endsWith <- booleanGen
    yield Charset(chars, length, repeat, startsWith, endsWith)

  val charsetsGen: Gen[List[Charset[Iterable[Char]]]] =
    for
      size <- Gen.choose(1, 4)
      charsets <- chars.take(size).map(charsetGen).sequence
    yield charsets

  val consecutiveGen: Gen[Consecutive] =
    for
      max <- Gen.choose(1, 8)
      step <- Gen.choose(0, 3)
      caseSensitive <- booleanGen
    yield Consecutive(max, step, caseSensitive)

  val charsetsGenGen: Gen[CharsetsGen[Iterable[Char]]] =
    for
      charsets <- charsetsGen
      elementSum = charsets.map { charset =>
        if charset.repeat then charset.length
        else charset.length & Interval.atOrBelow(charset.chars.size)
      }.combineAll
      lowerUpperBound = elementSum.upperBound match
        case upperBound: ValueBound[_] => upperBound.upper
        case _ => 64
      upperLowerBound = elementSum.lowerBound match
        case lowerBound: ValueBound[_] => lowerBound.lower
        case _ => 1
      closeLower <- booleanGen
      lower <- Gen.choose(1, if closeLower then lowerUpperBound else lowerUpperBound - 1)
      closeUpper <- booleanGen
      upperMin =
        if closeUpper && closeLower then upperLowerBound max lower
        else if closeUpper then upperLowerBound max (lower + 1)
        else if closeLower then (upperLowerBound + 1) max (lower + 1)
        else (upperLowerBound + 1) max (lower + 2)
      upper <- Gen.choose(upperMin, upperMin max 128)
      consecutiveOption <- Gen.oneOf(Gen.const(none[Consecutive]), consecutiveGen.map(_.some))
      retry <- Gen.choose(128, 256)
    yield CharsetsGen(charsets, Interval.fromBounds(
      if closeLower then Closed(lower) else Open(lower),
      if closeUpper then Closed(upper) else Open(upper)
    ), consecutiveOption, retry)

  val charsetsGenSeedGen: Gen[(CharsetsGen[Iterable[Char]], Random[Id])] =
    for
      charsetsGen <- charsetsGenGen
      seed <- Gen.long
    yield (charsetsGen, IdRandom(seed))

  property("should generate a random string") = forAll (charsetsGenSeedGen) { case (charsetsGen, random) =>
    charsetsGen(random) match
      case Left(error) =>
        println(error)
        false
      case Right(_) => true
  }

end GenCharsetsSpecification
