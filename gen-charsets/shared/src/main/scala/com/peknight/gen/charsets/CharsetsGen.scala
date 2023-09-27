package com.peknight.gen.charsets

import cats.data.{EitherT, StateT}
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import cats.{Foldable, Monad, Monoid}
import com.peknight.error.spire.math.IntervalEmptyError
import com.peknight.error.spire.math.interval.UnboundError
import com.peknight.error.std.Error
import com.peknight.gen.charsets.CharsetsGen.*
import com.peknight.gen.charsets.CharsetsGen.StartEnd.{Both, End, Neither, Start}
import com.peknight.random.Random
import com.peknight.random.state.{between, nextIntBounded}
import com.peknight.spire.ext.syntax.bound.{lower, upper}
import com.peknight.validation.collection.iterableOnce.either.nonEmpty
import com.peknight.validation.collection.list.either.nonEmpty as listNonEmpty
import com.peknight.validation.spire.math.interval.either.{nonNegative, positive, nonEmpty as intervalNonEmpty}
import com.peknight.validation.traverse.either.traverse
import spire.math.Interval
import spire.math.interval.*
import spire.std.int.IntAlgebra

import scala.annotation.tailrec

case class CharsetsGen[+C <: Iterable[Char]](
                                             // 生成使用的字符集
                                             charsets: List[Charset[C]],
                                             // 生成长度区间
                                             length: Interval[Int],
                                             // 连续性限制
                                             consecutiveOption: Option[Consecutive] = None,
                                             // 生成失败重试次数
                                             retry: Int = 3
                                           ):

  def apply[F[_]: Monad](random: Random[F]): F[Either[Error, String]] =
    StateT.get[F, Random[F]].liftS.flatMap { random => (
      for
        // 参数检查
        _ <- checkConsecutive(consecutiveOption).liftE
        _ <- nonNegative(retry, "retry").liftE
        charsets <- listNonEmpty(charsets, "charsets").liftE
        // 各字符集长度区间检查
        charsets <- traverse(charsets, "charsets")(checkCharset).liftE
        // 细化长度区间
        tuple = calculateLengths(charsets.zipWithIndex.map(_.swap).toList.toMap,
          length & Interval.atOrAbove(1), false)
        // 求全局区间与和区间交集并检查存在上限
        global <- checkLength(tuple._2, "global").flatMap(checkBounded).liftE
        charsetMap <- checkStartEnd(tuple._1, global).liftE
        // 生成
        result <- generate(charsetMap, global)
      yield result
    ).leftMap(random *: _) }.value.runA(random)

  private[this] def generate[F[_]: Monad](charsets: Map[Int, Charset[Vector[Char]]], global: Interval[Int])
  : StateEitherT[F, String] =
    // 全局区间上限
    val result =
      for
        // 生成字符串长度
        length <- betweenInterval(global).liftS
        // 逐个生成字符
        res <- generate(calculateLengths(charsets, length, false), length)
      yield res
    // 失败重试
    Monad[[A] =>> StateEitherT[F, A]].tailRecM(retry) { retry =>
      if retry <= 0 then result.map(_.asRight)
      else EitherT(result.fold(_ => (retry - 1).asLeft[String].asRight[Error], _.asRight[Int].asRight[Error]))
    }

  private[this] def generate[F[_]: Monad](charsets: Map[Int, Charset[Vector[Char]]], length: Int)
  : StateEitherT[F, String] =
    Monad[[A] =>> StateEitherT[F, A]].tailRecM((List.empty[Char], List.empty[Char], charsets, length)) {
      case context @ (chars, consecutiveChars, charsets, remain) =>
        // 待生成长度为0，生成结束，返回结果
        if remain == 0 then chars.reverse.mkString.asRight.pure else
          // 过滤掉无法出现在下一位的字符、字符集
          val charMap = filterChars(charsets, consecutiveOption, consecutiveChars, chars.isEmpty, remain == 1)
          for
            // 检查字符集不为空
            charMap <- nonEmpty(charMap, "charMap").left.map(context *: _).liftE
            // 随机取字符集
            index <- nextIndex(charMap.size).liftS
            key = charMap.keys.toVector(index)
            charVector = charMap(key)
            // 随机取字符
            index <- nextIndex(charVector.size).liftS
            ch = charVector(index)
          yield
            // 剩余字符数
            val nextRemain = remain - 1
            // 当前生成使用的字符集
            val charset = charsets(key)
            // 细化各字符集长度区间
            val nextCharsets = calculateLengths(
              charsets + (key -> charset.copy(
                chars = nextChars(ch, charset),
                length = (charset.length - 1) & Interval.atOrAbove(0)
              )),
              nextRemain,
              true
            )
            // 中间值更新
            (ch :: chars, nextConsecutiveChars(ch, consecutiveChars, consecutiveOption), nextCharsets, nextRemain)
              .asLeft
    }

end CharsetsGen
object CharsetsGen:

  private[charsets] type StateEitherT[F[_], A] = EitherT[[S] =>> StateT[F, Random[F], S], Error, A]

  extension[A] (either: Either[Error, A])
    private[charsets] def liftE[F[_] : Monad]: StateEitherT[F, A] =
      EitherT(either.pure[[S] =>> StateT[F, Random[F], S]])
  end extension

  extension[F[_] : Monad, A] (state: StateT[F, Random[F], A])
    private[charsets] def liftS: StateEitherT[F, A] = EitherT(state.map(_.asRight[Error]))
  end extension

  given Monoid[Interval[Int]] with
    def empty: Interval[Int] = Interval.point(0)

    def combine(x: Interval[Int], y: Interval[Int]): Interval[Int] = x + y
  end given

  extension[G[_] : Foldable] (intervals: G[Interval[Int]])
    private[charsets] def combineAll: Interval[Int] = Foldable[G].fold(intervals)
  end extension

  extension[K, C <: Iterable[Char]] (charsets: Map[K, Charset[C]])
    private[charsets] def combineLengths: Interval[Int] = Foldable[List].fold(charsets.values.toList.map(_.length))
  end extension

  private[charsets] enum StartEnd derives CanEqual:
    case Both, Start, End, Neither
  end StartEnd

  def allocate[F[_] : Monad, K](global: Interval[Int], elements: Map[K, Interval[Int]])
  : Either[Error, StateT[F, Random[F], Map[K, Int]]] =
    for
      elements <- listNonEmpty(elements.toList, "elements")
      elements <- traverse(elements, "elements") {
        case (k, length) => checkLength(length, "length").map((k, _)).left.map(k *: _)
      }
      elementSum = elements.map(_._2).combineAll
      global <- checkLength(global & elementSum & Interval.atOrAbove(1), "global").flatMap(checkBounded)
        .left.map(elementSum *: _)
    yield
      Monad[[A] =>> StateT[F, Random[F], A]].tailRecM((Map.empty[K, Int], elements.toList.toMap, global)) {
        case (map, remain, global) =>
          if remain.isEmpty then map.asRight.pure else
            for
              index <- nextIndex(remain.size)
              key = remain.keys.toVector(index)
              current = remain(key)
              nextRemain = remain - key
              remainSum = nextRemain.values.toList.combineAll
              len <- betweenInterval(current & (global - remainSum))
            yield (map + (key -> len), nextRemain, remainSum & (global - len)).asLeft
      }

  private[charsets] def nextIndex[F[_] : Monad](size: Int): StateT[F, Random[F], Int] =
    if size == 1 then 0.pure else nextIntBounded(size)

  private[charsets] def betweenInterval[F[_] : Monad](interval: Interval[Int]): StateT[F, Random[F], Int] =
    val lower = interval.lowerBound match
      case lowerBound: ValueBound[_] => lowerBound.lower
      case _ => 0
    val upper = interval.upperBound match
      case upperBound: ValueBound[_] => upperBound.upper
      case _ => Int.MaxValue
    if upper > lower then between(lower, upper + 1) else lower.pure

  private[charsets] def calculateLengths[C <: Iterable[Char]](charsets: Map[Int, Charset[C]], length: Int,
                                                              started: Boolean): Map[Int, Charset[C]] =
    calculateLengths(charsets, Interval.point(length), started)._1

  private[charsets] def calculateLengths[C <: Iterable[Char]](charsets: Map[Int, Charset[C]],
                                                              global: Interval[Int], started: Boolean)
  : (Map[Int, Charset[C]], Interval[Int]) =
    @tailrec def go(charsets: Map[StartEnd, Map[Int, Charset[C]]], global: Interval[Int], started: Boolean,
                    tailRecEnd: Boolean) : (Map[StartEnd, Map[Int, Charset[C]]], Interval[Int]) =
      if tailRecEnd then (charsets, global) else
        val (nextCharsets, nextGlobal, modified) =
          charsets.foldLeft((charsets, global, false)) {
            case ((charsets, global, charsetsModified), (startEnd, subCharsets)) =>
              val nonStartEndLength = StartEnd.values.toList.filter(_ != startEnd)
                .map(charsets.getOrElse(_, Map.empty).combineLengths)
                .combineAll
              val (nextSubCharsets, nextGlobal, subModified) =
                subCharsets.foldLeft((subCharsets, global, false)) {
                  case ((subCharsets, global, subModified), (key, charset)) =>
                    val otherStartEndMap = subCharsets - key
                    val otherStartEndLength = otherStartEndMap.combineLengths
                    val otherLength = otherStartEndLength + nonStartEndLength
                    val startEndLength = startEnd match
                      case Both if started =>
                        val onlyEndMap = charsets.getOrElse(End, Map.empty)
                        Interval.atOrAbove(1) - (otherStartEndMap ++ onlyEndMap).combineLengths
                      case Both =>
                        val onlyStartMap = charsets.getOrElse(Start, Map.empty)
                        val onlyEndMap = charsets.getOrElse(End, Map.empty)
                        (Interval.atOrAbove(1) - (otherStartEndMap ++ onlyStartMap).combineLengths) &
                          (Interval.atOrAbove(1) - (otherStartEndMap ++ onlyEndMap).combineLengths)
                      case Start if started => Interval.all
                      case Start | End =>
                        val bothMap = charsets.getOrElse(Both, Map.empty)
                        Interval.atOrAbove(1) - (otherStartEndMap ++ bothMap).combineLengths
                      case Neither => Interval.all
                    val nextLength = charset.length & (global - otherLength) & startEndLength
                    val nextGlobal = global & (nextLength + otherLength)
                    if nextLength === charset.length then
                      (subCharsets, nextGlobal, subModified)
                    else
                      (subCharsets + (key -> charset.copy(length = nextLength)), nextGlobal, true)
                }
              if subModified then
                (charsets + (startEnd -> nextSubCharsets), nextGlobal, true)
              else
                (charsets, nextGlobal, charsetsModified)
          }
        val finalGlobal =
          if !started && (nextCharsets.get(Both).map(_.combineLengths).combineAll & Interval.atOrAbove(1)).isEmpty then
            nextGlobal & Interval.atOrAbove(2)
          else nextGlobal & Interval.atOrAbove(1)
        println(s"global=$global")
        println(s"charsets=$charsets")
        println(s"nextGlobal=$nextGlobal")
        println(s"nextCharsets=$nextCharsets")
        go(nextCharsets, nextGlobal, started, !modified && finalGlobal === global)
    val (startEndCharsets, nextGlobal) = go(groupByStartEnd(charsets), global, started, false)
    println("======")
    (startEndCharsets.values.foldLeft(Map.empty[Int, Charset[C]])(_ ++ _), nextGlobal)


  private[charsets] def resetLengths(charsets: Map[Int, Charset[Vector[Char]]], length: Int)
  : Map[Int, Charset[Vector[Char]]] =
    val global = Interval.point(length)
    val lengths = charsets.map((k, charset) => (k, charset.length))
    charsets.foldLeft(charsets) { case (charsets, (k, charset)) =>
      charsets + (k -> charset.copy(length = charset.length & (global - (lengths - k).values.toList.combineAll)))
    }

  private[charsets] def filterChars(charsets: Map[Int, Charset[Vector[Char]]], consecutiveOption: Option[Consecutive],
                                    consecutiveChars: List[Char], start: Boolean, end: Boolean): Map[Int, Vector[Char]] =
    val endCharsets = charsets.filter((_, charset) =>
      charset.endsWith && (charset.length & Interval.atOrAbove(1)).nonEmpty
    )
    val filterEndKeyOption = endCharsets.combineLengths.upperBound match
      case upperBound: ValueBound[_] if upperBound.upper <= 1 => endCharsets.keys.headOption
      case _ => none[Int]
    charsets.filter((k, charset) => !charset.length.isAt(0) &&
        (!start || charset.startsWith) &&
        (!end || charset.endsWith) &&
        (!filterEndKeyOption.contains(k) || end))
      .map((k, charset) => (k, consecutiveOption
        .filter(consecutive => consecutive.max == consecutiveChars.length)
        .map(consecutive => charset.chars.filter(ch => !overConsecutiveLimit(ch, consecutiveChars, consecutive)))
        .getOrElse(charset.chars)
      ))
      .filter((_, chars) => chars.nonEmpty)

  private[charsets] def nextChars(ch: Char, charset: Charset[Vector[Char]]): Vector[Char] =
    if charset.repeat then charset.chars else
      charset.chars.foldLeft((Vector.empty[Char], false)) { case ((acc, flag), c) =>
        if flag then (acc :+ c, true)
        else if c == ch then (acc, true)
        else (acc :+ c, false)
      }._1

  private[charsets] def calculateStep(a: Char, b: Char, caseSensitive: Boolean): Option[Int] =
    if a.isLetter && b.isLetter && caseSensitive then Some(a.toLower - b.toLower)
    else if (a.isDigit && b.isDigit) || (a.isLetter && b.isLetter) then Some(a - b)
    else None

  private[charsets] def overConsecutiveLimit(ch: Char, consecutiveChars: List[Char], consecutive: Consecutive): Boolean =
    consecutiveChars match
      case _ if consecutiveChars.length < consecutive.max => false
      case _ if consecutiveChars.length > consecutive.max => true
      case first :: second :: tail =>
        calculateStep(first, second, consecutive.caseSensitive)
          .flatMap(stepAcc => calculateStep(ch, first, consecutive.caseSensitive)
            .map(step => step == stepAcc))
          .getOrElse(false)
      case head :: Nil =>
        calculateStep(ch, head, consecutive.caseSensitive).exists(step => step.abs <= consecutive.step)
      case _ => false

  private[charsets] def nextConsecutiveChars(ch: Char, consecutiveChars: List[Char],
                                             consecutiveOption: Option[Consecutive]): List[Char] =
    if ch.isLetterOrDigit then
      (consecutiveOption, consecutiveChars) match
        case (Some(consecutive), first :: second :: tail) =>
          calculateStep(first, second, consecutive.caseSensitive).fold(List(ch)) { stepAcc =>
            calculateStep(ch, first, consecutive.caseSensitive).fold(List(ch)) { step =>
              if step == stepAcc then ch :: consecutiveChars
              else if step.abs <= consecutive.step then List(ch, first)
              else List(ch)
            }
          }
        case (Some(consecutive), head :: Nil) =>
          calculateStep(ch, head, consecutive.caseSensitive).fold(List(ch)) { step =>
            if step.abs <= consecutive.step then ch :: consecutiveChars
            else List(ch)
          }
        case (Some(_), Nil) => List(ch)
        case _ => List.empty
    else List.empty

  private[charsets] def checkLength(length: Interval[Int], label: String): Either[Error, Interval[Int]] =
    intervalNonEmpty(length, label).left.map(length *: _)

  private[charsets] def checkBounded(length: Interval[Int]): Either[Error, Interval[Int]] =
    length.upperBound match
      case _: ValueBound[_] => length.asRight[Error]
      case _ => (length *: UnboundError("upperBound")).asLeft[Interval[Int]]

  private[charsets] def checkCharset[C <: Iterable[Char]](charset: Charset[C]): Either[Error, Charset[Vector[Char]]] = {
    for
      chars <- nonEmpty(charset.chars, "chars")
      length <-
        if charset.repeat then checkLength(charset.length & Interval.atOrAbove(0), "length")
        else checkLength(charset.length & Interval.closed(0, chars.size), "length")
      length <- if length.isAt(0) then IntervalEmptyError("length").asLeft[Interval[Int]] else length.asRight[Error]
    yield
      charset.copy(chars = chars.toVector, length = length)
  }.left.map(charset *: _)

  private[charsets] def groupByStartEnd[C <: Iterable[Char]](charsets: Map[Int, Charset[C]])
  : Map[StartEnd, Map[Int, Charset[C]]] =
    charsets.groupBy((_, charset) =>
      if charset.startsWith && charset.endsWith then Both
      else if charset.startsWith then Start
      else if charset.endsWith then End
      else Neither
    )

  private[charsets] def checkStartEnd[C <: Iterable[Char]](charsets: Map[Int, Charset[C]], global: Interval[Int])
  : Either[Error, Map[Int, Charset[C]]] =
    val startEndCharsets = groupByStartEnd(charsets)
    val bothMap = startEndCharsets.getOrElse(Both, Map.empty)
    if (global & Interval.atOrAbove(1)).isAt(1) then
      nonEmpty(bothMap, "bothStartEndCharsets").map(_ => charsets)
    else
      val onlyStartMap = startEndCharsets.getOrElse(Start, Map.empty)
      val onlyEndMap = startEndCharsets.getOrElse(End, Map.empty)
      if onlyStartMap.isEmpty && onlyEndMap.isEmpty && bothMap.size == 1 then
        intervalNonEmpty(bothMap.head._2.length & Interval.atOrAbove(2), "bothStartEndCharsets").map(_ => charsets)
      else if bothMap.isEmpty then
        for
          _ <- nonEmpty(onlyStartMap, "startCharsets")
          _ <- nonEmpty(onlyEndMap, "endCharsets")
        yield charsets
      else charsets.asRight

  private[charsets] def checkConsecutive(consecutiveOption: Option[Consecutive]): Either[Error, Option[Consecutive]] =
    consecutiveOption.fold(consecutiveOption.asRight[Error]) { consecutive =>
      for
        _ <- positive(consecutive.max, "max")
        _ <- nonNegative(consecutive.step, "step")
      yield consecutiveOption
    }
end CharsetsGen