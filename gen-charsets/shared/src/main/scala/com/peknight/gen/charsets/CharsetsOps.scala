package com.peknight.gen.charsets

import cats.data.{EitherT, StateT}
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.{Foldable, Monad, Monoid, Show}
import com.peknight.error.Error
import com.peknight.error.spire.math.interval.IntervalEmpty
import com.peknight.error.syntax.either.{label, prepended}
import com.peknight.gen.charsets.CharsetsOps.StartEnd.{Both, End, Neither, Start}
import com.peknight.random.Random
import com.peknight.random.state.{between, nextIntBounded}
import com.peknight.spire.ext.syntax.bound.{lower, upper}
import com.peknight.spire.ext.syntax.interval.close
import com.peknight.validation.collection.iterableOnce.either.nonEmpty
import com.peknight.validation.collection.list.either.nonEmpty as listNonEmpty
import com.peknight.validation.spire.math.interval.either.{nonNegative, positive, upperValueBounded, nonEmpty as intervalNonEmpty}
import com.peknight.validation.traverse.either.traverse
import spire.math.Interval
import spire.math.interval.*
import spire.std.int.IntAlgebra

import scala.annotation.tailrec

private[charsets] object CharsetsOps:

  private[charsets] enum StartEnd derives CanEqual:
    case Both, Start, End, Neither
  end StartEnd

  private type StateEitherT[F[_], A] = EitherT[[S] =>> StateT[F, Random[F], S], Error, A]

  extension [A] (either: Either[Error, A])
    private def liftE[F[_] : Monad]: StateEitherT[F, A] =
      EitherT(either.pure[[S] =>> StateT[F, Random[F], S]])
  end extension

  extension [F[_]: Monad, A] (state: StateT[F, Random[F], A])
    private def liftS: StateEitherT[F, A] = EitherT(state.map(_.asRight[Error]))
  end extension

  given Monoid[Interval[Int]] with
    def empty: Interval[Int] = Interval.point(0)
    def combine(x: Interval[Int], y: Interval[Int]): Interval[Int] = x + y
  end given

  def generate[F[_]: Monad](charsets: Charsets): StateT[F, Random[F], Either[Error, String]] =
    given Show[Random[F]] = Show.fromToString
    StateT.get[F, Random[F]].liftS.flatMap { random => {
      for
        // 参数检查
        consecutiveOption <- checkConsecutive(charsets.consecutiveOption).liftE
        retry <- nonNegative(charsets.retry).label("retry").liftE
        cs <- listNonEmpty(charsets.charsets).label("charsets").liftE
        // 各字符集长度区间检查
        cs <- traverse(cs)(checkCharset).label("charsets").liftE
        // 细化长度区间
        tuple = calculateLengths(cs.zipWithIndex.map(_.swap).toList.toMap,
          charsets.length.close & Interval.atOrAbove(1), false)
        // 求全局区间与和区间交集并检查存在上限
        global <- intervalNonEmpty(tuple._2).flatMap(upperValueBounded).label("global").liftE
        cs <- checkStartEnd(tuple._1, global).liftE
        // 生成
        result <- generate(cs, global, consecutiveOption, retry)
      yield result
    }.leftMap(random *: _) }.value

  private def generate[F[_] : Monad](charsets: Map[Int, Charset], global: Interval[Int],
                                     consecutiveOption: Option[Consecutive], retry: Int): StateEitherT[F, String] =
    // 全局区间上限
    val result =
      for
      // 生成字符串长度
        length <- betweenInterval(global).liftS
        // 逐个生成字符
        res <- generate(calculateLengths(charsets, length, false), length, consecutiveOption)
      yield res
    // 失败重试
    Monad[[A] =>> StateEitherT[F, A]].tailRecM(retry) { retry =>
      if retry <= 0 then result.map(_.asRight)
      else EitherT(result.fold(_ => (retry - 1).asLeft[String].asRight[Error], _.asRight[Int].asRight[Error]))
    }

  private def generate[F[_] : Monad](charsets: Map[Int, Charset], length: Int,
                                     consecutiveOption: Option[Consecutive]): StateEitherT[F, String] =
    given Show[(List[Char], List[Char], Map[Int, Charset], Int)] = Show.fromToString
    Monad[[A] =>> StateEitherT[F, A]].tailRecM((List.empty[Char], List.empty[Char], charsets, length)) {
      case context @ (chars, consecutiveChars, charsets, remain) =>
        // 待生成长度为0，生成结束，返回结果
        if remain == 0 then chars.reverse.mkString.asRight.pure else
          // 过滤掉无法出现在下一位的字符、字符集
          val charMap = filterChars(charsets, remain, consecutiveOption, consecutiveChars, chars.isEmpty, remain == 1)
          for
          // 检查字符集不为空
            charMap <- nonEmpty(charMap).label("charMap").prepended(context).liftE
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

  def allocate[F[_] : Monad, K: Show](global: Interval[Int], elements: Map[K, Interval[Int]])
  : Either[Error, StateT[F, Random[F], Map[K, Int]]] =
    given Show[Interval[Int]] = Show.fromToString
    for
      elements <- listNonEmpty(elements.toList).label("elements")
      elements <- traverse(elements) {
        case (k, length) => intervalNonEmpty(length).label("length").map((k, _)).prepended(k)
      }.label("elements")
      elementSum = combineAll(elements.map(_._2))
      global <- intervalNonEmpty(global & elementSum & Interval.atOrAbove(1)).flatMap(upperValueBounded)
        .label("global").prepended(elementSum)
    yield
      Monad[[A] =>> StateT[F, Random[F], A]].tailRecM((Map.empty[K, Int], elements.toList.toMap, global)) {
        case (map, remain, global) =>
          if remain.isEmpty then map.asRight.pure else
            for
              index <- nextIndex(remain.size)
              key = remain.keys.toVector(index)
              current = remain(key)
              nextRemain = remain - key
              remainSum = combineAll(nextRemain.values.map(_ & Interval.atOrAbove(0)).toList)
              len <- betweenInterval(current & (global - remainSum) & Interval.atOrAbove(0))
            yield (map + (key -> len), nextRemain, remainSum & (global - len)).asLeft
      }

  private def nextIndex[F[_] : Monad](size: Int): StateT[F, Random[F], Int] =
    if size == 1 then 0.pure else nextIntBounded(size)

  private def betweenInterval[F[_] : Monad](interval: Interval[Int]): StateT[F, Random[F], Int] =
    val lower = interval.lowerBound match
      case lowerBound: ValueBound[_] => lowerBound.lower
      case _ => 0
    val upper = interval.upperBound match
      case upperBound: ValueBound[_] => upperBound.upper
      case _ => Int.MaxValue
    if upper > lower then between(lower, upper + 1) else lower.pure

  private def calculateLengths[C <: Iterable[Char]](charsets: Map[Int, Charset], length: Int, started: Boolean)
  : Map[Int, Charset] =
    calculateLengths(charsets, Interval.point(length), started)._1

  private def calculateLengths[C <: Iterable[Char]](charsets: Map[Int, Charset], global: Interval[Int],
                                                    started: Boolean): (Map[Int, Charset], Interval[Int]) =
    @tailrec def go(charsets: Map[StartEnd, Map[Int, Charset]], global: Interval[Int], started: Boolean,
                    tailRecEnd: Boolean) : (Map[StartEnd, Map[Int, Charset]], Interval[Int]) =
      if tailRecEnd then (charsets, global) else
        val (nextCharsets, nextGlobal, modified) =
          charsets.foldLeft((charsets, global, false)) {
            case ((charsets, global, charsetsModified), (startEnd, subCharsets)) =>
              val nonStartEndLength = combineAll(StartEnd.values.toList.filter(_ != startEnd)
                .map(startEnd => combineAll(charsets.getOrElse(startEnd, Map.empty))))
              val (nextSubCharsets, nextGlobal, subModified) =
                subCharsets.foldLeft((subCharsets, global, false)) {
                  case ((subCharsets, global, subModified), (key, charset)) =>
                    val otherStartEndMap = subCharsets - key
                    val otherStartEndLength = combineAll(otherStartEndMap)
                    val otherLength = otherStartEndLength + nonStartEndLength
                    val startEndLength = startEnd match
                      case _ if global.isAt(0) => Interval.all
                      case Both if started =>
                        val onlyEndMap = charsets.getOrElse(End, Map.empty)
                        Interval.atOrAbove(1) - combineAll(otherStartEndMap ++ onlyEndMap)
                      case Both =>
                        val onlyStartMap = charsets.getOrElse(Start, Map.empty)
                        val onlyEndMap = charsets.getOrElse(End, Map.empty)
                        (Interval.atOrAbove(1) - combineAll(otherStartEndMap ++ onlyStartMap)) &
                          (Interval.atOrAbove(1) - combineAll(otherStartEndMap ++ onlyEndMap))
                      case Start if started => Interval.all
                      case Start | End =>
                        val bothMap = charsets.getOrElse(Both, Map.empty)
                        Interval.atOrAbove(1) - combineAll(otherStartEndMap ++ bothMap)
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
          if !started && (combineAll(nextCharsets.get(Both).map(combineAll)) & Interval.atOrAbove(1)).isEmpty then
            nextGlobal & Interval.atOrAbove(2)
          else if started && nextGlobal.isAt(0) then nextGlobal
          else nextGlobal & Interval.atOrAbove(1)
        go(nextCharsets, finalGlobal, started, !modified && finalGlobal === global)
    val (startEndCharsets, nextGlobal) = go(groupByStartEnd(charsets), global, started, false)
    (startEndCharsets.values.foldLeft(Map.empty[Int, Charset])(_ ++ _), nextGlobal)

  private def filterChars(charsets: Map[Int, Charset], length: Int, consecutiveOption: Option[Consecutive],
                          consecutiveChars: List[Char], start: Boolean, end: Boolean): Map[Int, String] =
    val endCharsets = charsets.filter((_, charset) =>
      charset.endsWith && (charset.length & Interval.atOrAbove(1)).nonEmpty
    )
    val nonEndLength = combineAll(charsets.filter((_, charset) => !charset.endsWith))
    val filterEndKeys = ((Interval.point(length) - nonEndLength) & combineAll(endCharsets)).upperBound match
      case upperBound: ValueBound[_] if upperBound.upper <= 1 => endCharsets.keys.toList
      case _ => List.empty[Int]
    charsets.filter((k, charset) => !charset.length.isAt(0) &&
        (!start || charset.startsWith) &&
        (!end || charset.endsWith) &&
        (!filterEndKeys.contains(k) || end))
      .map((k, charset) => (k, consecutiveOption
        .filter(consecutive => consecutive.max == consecutiveChars.length)
        .map(consecutive => charset.chars.filter(ch => !overConsecutiveLimit(ch, consecutiveChars, consecutive)))
        .getOrElse(charset.chars)
      ))
      .filter((_, chars) => chars.nonEmpty)

  private def nextChars(ch: Char, charset: Charset): String =
    if charset.repeat then charset.chars else
      charset.chars.foldLeft((Vector.empty[Char], false)) { case ((acc, flag), c) =>
        if flag then (acc :+ c, true)
        else if c == ch then (acc, true)
        else (acc :+ c, false)
      }._1.mkString

  private def calculateStep(a: Char, b: Char, caseSensitive: Boolean): Option[Int] =
    if a.isLetter && b.isLetter && caseSensitive then Some(a.toLower - b.toLower)
    else if (a.isDigit && b.isDigit) || (a.isLetter && b.isLetter) then Some(a - b)
    else None

  private def overConsecutiveLimit(ch: Char, consecutiveChars: List[Char], consecutive: Consecutive): Boolean =
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

  private def nextConsecutiveChars(ch: Char, consecutiveChars: List[Char], consecutiveOption: Option[Consecutive])
  : List[Char] =
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

  private def groupByStartEnd[K](charsets: Map[K, Charset])
  : Map[StartEnd, Map[K, Charset]] =
    charsets.groupBy((_, charset) =>
      if charset.startsWith && charset.endsWith then Both
      else if charset.startsWith then Start
      else if charset.endsWith then End
      else Neither
    )

  private def combineAll[G[_] : Foldable](intervals: G[Interval[Int]]): Interval[Int] =
    Foldable[G].fold(intervals)

  private def combineAll[K](charsets: Map[K, Charset]): Interval[Int] =
    Foldable[List].fold(charsets.values.toList.map(_.length))

  private[charsets] def combineStartEnd[K](charsets: Map[K, Charset]): Interval[Int] =
    val startEndMap = groupByStartEnd(charsets)
    val bothLength = combineAll(startEndMap.getOrElse(Both, Map.empty)) & Interval.atOrAbove(0)
    val onlyStartLength = combineAll(startEndMap.getOrElse(Start, Map.empty)) & Interval.atOrAbove(0)
    val onlyEndLength = combineAll(startEndMap.getOrElse(End, Map.empty)) & Interval.atOrAbove(0)
    val neitherLength = combineAll(startEndMap.getOrElse(Neither, Map.empty)) & Interval.atOrAbove(0)
    (bothLength.lowerBound, onlyStartLength.lowerBound, onlyEndLength.lowerBound) match
      case (both: ValueBound[Int], _, _) if both.lower >= 1 =>
        bothLength + onlyStartLength + onlyEndLength + neitherLength
      case (_, onlyStart: ValueBound[Int], onlyEnd: ValueBound[Int]) if onlyStart.lower >= 1 && onlyEnd.lower >= 1 =>
        bothLength + onlyStartLength + onlyEndLength + neitherLength
      case (_, onlyStart: ValueBound[Int], _) if onlyStart.lower >= 1 =>
        ((bothLength + onlyEndLength) & Interval.atOrAbove(1)) + onlyStartLength + neitherLength
      case (_, _, onlyEnd: ValueBound[Int]) if onlyEnd.lower >= 1 =>
        ((bothLength + onlyStartLength) & Interval.atOrAbove(1)) + onlyEndLength + neitherLength
      case _ if bothLength.isAt(0) =>
        bothLength + (onlyStartLength & Interval.atOrAbove(1)) + (onlyEndLength & Interval.atOrAbove(1)) + neitherLength
      case _ if onlyStartLength.isAt(0) || onlyEndLength.isAt(0) =>
        (bothLength & Interval.atOrAbove(1)) + onlyStartLength + onlyEndLength + neitherLength
      case _ =>
        bothLength + (onlyStartLength & Interval.atOrAbove(1)) + (onlyEndLength & Interval.atOrAbove(1)) + neitherLength

  private def checkCharset(charset: Charset): Either[Error, Charset] =
    val either =
      for
        chars <- nonEmpty(charset.chars).label("chars")
        length <-
          if charset.repeat then intervalNonEmpty(charset.length & Interval.atOrAbove(0)).label("length")
          else intervalNonEmpty(charset.length & Interval.closed(0, chars.length)).label("length")
        length <-
          if length.isAt(0) then IntervalEmpty.label("length").asLeft[Interval[Int]]
          else length.asRight[Error]
      yield
        charset.copy(chars = chars.toString, length = length.close)
    given Show[Charset] = Show.fromToString
    either.prepended(charset)

  private def checkStartEnd(charsets: Map[Int, Charset], global: Interval[Int])
  : Either[Error, Map[Int, Charset]] =
    val startEndCharsets = groupByStartEnd(charsets)
    val bothMap = startEndCharsets.getOrElse(Both, Map.empty)
    if (global & Interval.atOrAbove(1)).isAt(1) then
      nonEmpty(bothMap).label("bothStartEndCharsets").map(_ => charsets)
    else
      val onlyStartMap = startEndCharsets.getOrElse(Start, Map.empty)
      val onlyEndMap = startEndCharsets.getOrElse(End, Map.empty)
      if onlyStartMap.isEmpty && onlyEndMap.isEmpty && bothMap.size == 1 then
        intervalNonEmpty(bothMap.head._2.length & Interval.atOrAbove(2)).label("bothStartEndCharsets")
          .map(_ => charsets)
      else if bothMap.isEmpty then
        for
          _ <- nonEmpty(onlyStartMap).label("startCharsets")
          _ <- nonEmpty(onlyEndMap).label("endCharsets")
        yield charsets
      else charsets.asRight

  private def checkConsecutive(consecutiveOption: Option[Consecutive]): Either[Error, Option[Consecutive]] =
    traverse(consecutiveOption) { consecutive =>
      for
        _ <- positive(consecutive.max).label("max")
        _ <- nonNegative(consecutive.step).label("step")
      yield consecutive
    }
end CharsetsOps
