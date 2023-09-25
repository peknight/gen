package com.peknight.gen.charsets

import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.{Id, Monad}
import com.peknight.cats.instances.scalacheck.gen.given
import com.peknight.error.std.Error
import com.peknight.gen.charsets.CharsetsGen.combineAll
import com.peknight.random.Random
import com.peknight.random.id.Random as IdRandom
import com.peknight.spire.ext.syntax.bound.{lower, upper}
import org.scalacheck.rng.Seed
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
      upper <- Gen.choose(if close then 1 else 2, 32)
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
  def charsetsRepeatIntervalGen(lowerLowerBound: Int, lowerUpperBound: Int): Gen[Interval[Int]] =
    for
      closeLower <- booleanGen
      lower <- Gen.choose(
        if closeLower then lowerLowerBound else lowerLowerBound - 1,
        if closeLower then lowerUpperBound else lowerUpperBound - 1
      )
      closeUpper <- booleanGen
      upperMin =
        if closeUpper && closeLower then lower max 1
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

  val charsetsGen: Gen[List[Charset[Iterable[Char]]]] =
    for
      size <- Gen.choose(1, 4)
      charsets <- Monad[Gen].tailRecM((List.empty[Charset[Iterable[Char]]], chars.take(size), false, false)) {
        case (acc, Nil, _, _) => acc.reverse.asRight.pure
        case (acc, head :: tail, start, end) =>
          for
            repeat <- booleanGen
            startsWith <- if tail.isEmpty && !start then Gen.const(true) else booleanGen
            endsWith <- if tail.isEmpty && !end then Gen.const(true) else booleanGen
            lowerLowerBound = if tail.nonEmpty || (start && end) then 0 else if start || end then 1 else 2
            length <- if repeat then charsetIntervalGen else charsetsRepeatIntervalGen(lowerLowerBound, chars.length)
          yield
            (Charset(head, length, repeat, startsWith, endsWith) :: acc, tail, start || startsWith, end || endsWith)
              .asLeft
      }
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
        if charset.repeat then charset.length & Interval.atOrAbove(0)
        else charset.length & Interval.atOrBelow(charset.chars.size)
      }.combineAll
      lowerUpperBound = elementSum.upperBound match
        case upperBound: ValueBound[_] => upperBound.upper
        case _ => 64
      upperLowerBound = elementSum.lowerBound match
        case lowerBound: ValueBound[_] => lowerBound.lower
        case _ => 1
      closeLower <- booleanGen
      lower <- Gen.choose(if closeLower then 1 else 0, if closeLower then lowerUpperBound else lowerUpperBound - 1)
      closeUpper <- booleanGen
      upperMin =
        if closeUpper && closeLower then upperLowerBound max lower
        else if closeUpper then upperLowerBound max (lower + 1)
        else if closeLower then (upperLowerBound + 1) max (lower + 1)
        else (upperLowerBound + 1) max (lower + 2)
      upper <- Gen.choose(upperMin, upperMin max 128)
      consecutiveOption <- Gen.oneOf(Gen.const(none[Consecutive]), consecutiveGen.map(_.some))
      retry <- Gen.const(0)
    yield CharsetsGen(charsets, Interval.fromBounds(
      if closeLower then Closed(lower) else Open(lower),
      if closeUpper then Closed(upper) else Open(upper)
    ), consecutiveOption, retry)

  val charsetsGenSeedGen: Gen[(CharsetsGen[Iterable[Char]], Random[Id])] =
    for
      charsetsGen <- charsetsGenGen
      seed <- Gen.long
    yield (charsetsGen, IdRandom(seed))

  def review(seed: String): Unit =
    val (charsetsGen, random) = charsetsGenSeedGen.pureApply(Gen.Parameters.default, Seed.fromBase64(seed).get)
    log(charsetsGen, random, charsetsGen(random))

  def log(charsetsGen: CharsetsGen[Iterable[Char]], random: Random[Id], result: Either[Error, String]): Unit =
    println("================Start================")
    println(s"charsetsGen=$charsetsGen")
    println(s"random=$random")
    println(s"result=$result")
    println("================ End ================")

  review("I0e9alocoEI6SHrmUlTt9BoZgShfMIDTXejIEI7SHvF=")

  // import org.scalacheck.Prop.forAll
  // property("should generate a random string") = forAll (charsetsGenSeedGen) { case (charsetsGen, random) =>
  //   charsetsGen(random) match
  //     case Right(_) => true
  //     case result =>
  //       // log(charsets, random, result)
  //       // false
  //       true
  // }

end GenCharsetsSpecification
