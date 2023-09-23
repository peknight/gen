package com.peknight.gen.charsets

import cats.data.{EitherT, StateT}
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.{Foldable, Monad, Monoid}
import com.peknight.error.spire.math.IntervalEmptyError
import com.peknight.error.spire.math.interval.UnboundError
import com.peknight.error.std.Error
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

  private[this] type EitherStateT[F[_], A] = EitherT[[S] =>> StateT[F, Random[F], S], Error, A]

  extension[A] (either: Either[Error, A])
    private[CharsetsGen] def liftE[F[_]: Monad]: EitherStateT[F, A] =
      EitherT(either.pure[[S] =>> StateT[F, Random[F], S]])
  end extension
  extension [F[_]: Monad, A] (state: StateT[F, Random[F], A])
    private[CharsetsGen] def liftS: EitherStateT[F, A] = EitherT(state.map(_.asRight[Error]))
  end extension

  given Monoid[Interval[Int]] with
    def empty: Interval[Int] = Interval.point(0)
    def combine(x: Interval[Int], y: Interval[Int]): Interval[Int] = x + y
  end given

  extension [G[_]: Foldable] (intervals: G[Interval[Int]])
    def combineAll: Interval[Int] = Foldable[G].fold(intervals)
  end extension

  def apply[F[_]: Monad](random: Random[F]): F[Either[Error, String]] =
    StateT.get[F, Random[F]].liftS.flatMap { random => (
      for
        // 参数检查
        _ <- checkConsecutive.liftE
        _ <- nonNegative(retry, "retry").liftE
        charsets <- listNonEmpty(charsets, "charsets").liftE
        // 各字符集长度区间检查
        charsets <- traverse(charsets, "charsets")(checkCharset).liftE
        // 长度区间求和
        charsetSum = charsets.map(_.length).combineAll
        // 求全局区间与和区间交集并检查存在上限
        global <- checkLength(length & charsetSum & Interval.atOrAbove(1), "global").flatMap(checkBounded)
          .left.map(charsetSum *: _).liftE
        // 生成
        result <- generate(charsets.zipWithIndex.map(_.swap).toList.toMap, global)
      yield result
    ).leftMap(random *: _) }.value.runA(random)

  private[this] def generate[F[_]: Monad](charsets: Map[Int, Charset[Vector[Char]]], global: Interval[Int])
  : EitherStateT[F, String] =
    // 全局区间上限
    val result =
      for
        // 生成字符串长度
        length <- betweenInterval(global).liftS
        // 逐个生成字符
        res <- generate(resetLengths(charsets, length), length)
      yield res
    // 失败重试
    Monad[[A] =>> EitherStateT[F, A]].tailRecM(retry) { retry =>
      if retry <= 0 then result.map(_.asRight)
      else EitherT(result.fold(_ => (retry - 1).asLeft[String].asRight[Error], _.asRight[Int].asRight[Error]))
    }

  private[this] def generate[F[_]: Monad](charsets: Map[Int, Charset[Vector[Char]]], length: Int)
  : EitherStateT[F, String] =
    Monad[[A] =>> EitherStateT[F, A]].tailRecM(GenAccumulation(List.empty, charsets, length, None)) {
      case context @ GenAccumulation(chars, charsets, remain, consecutiveAccOption) =>
        // 待生成长度为0，生成结束，返回结果
        if remain == 0 then chars.reverse.mkString.asRight.pure else
          // 过滤掉无法出现在下一位的字符、字符集
          val charMap = filterChars(charsets, consecutiveAccOption, chars.isEmpty, remain == 1)
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
            // TODO
            // println(s"key=$key,ch=$ch")
            // 细化各字符集长度区间
            val nextCharsets = resetLengths(
              charsets + (key -> charset.copy(
                chars = nextChars(ch, charset),
                length = (charset.length - 1) & Interval.atOrAbove(0)
              )),
              nextRemain
            )
            // 中间值更新
            GenAccumulation(ch :: chars, nextCharsets, nextRemain, nextConsecutiveAccumulation(ch, consecutiveAccOption))
              .asLeft
    }

  private[this] def resetLengths(charsets: Map[Int, Charset[Vector[Char]]], length: Int)
  : Map[Int, Charset[Vector[Char]]] =
    val global = Interval.point(length)
    val lengths = charsets.map((k, charset) => (k, charset.length))
    val res = charsets.foldLeft(charsets) { case (charsets, (k, charset)) =>
      charsets + (k -> charset.copy(length = charset.length & (global - (lengths - k).values.toList.combineAll)))
    }
    // TODO
    // given CanEqual[Map[Int, Charset[Vector[Char]]], Map[Int, Charset[Vector[Char]]]] = CanEqual.derived
    // println("==========")
    // println(s"${res == charsets}")
    // println(s"l=$length")
    // println(s"i=$charsets")
    // println(s"o=$res")
    res

  private[this] def betweenInterval[F[_]: Monad](interval: Interval[Int]): StateT[F, Random[F], Int] =
    val lower = interval.lowerBound match
      case lowerBound: ValueBound[_] => lowerBound.lower
      case _ => 0
    val upper = interval.upperBound match
      case upperBound: ValueBound[_] => upperBound.upper
      case _ => Int.MaxValue
    if upper > lower then between(lower, upper + 1) else lower.pure

  private[this] def nextIndex[F[_]: Monad](size: Int): StateT[F, Random[F], Int] =
    if size == 1 then 0.pure else nextIntBounded(size)

  private[this] def filterChars(charsets: Map[Int, Charset[Vector[Char]]],
                                consecutiveAccOption: Option[ConsecutiveAccumulation],
                                start: Boolean, end: Boolean): Map[Int, Vector[Char]] =
    charsets.filter((_, charset) => !charset.length.isAt(0) &&
        (!start || charset.startsWith) &&
        (!end || charset.endsWith))
      .map((k, charset) => (k, consecutiveOption
        .flatMap(consecutive => consecutiveAccOption
          .filter(acc => consecutive.max == acc.length)
          .map(acc => charset.chars.filter(ch => !isConsecutive(ch, consecutive, acc))))
        .getOrElse(charset.chars)
      ))
      .filter((_, chars) => chars.nonEmpty)

  private[this] def nextChars(ch: Char, charset: Charset[Vector[Char]]): Vector[Char] =
    if charset.repeat then charset.chars else
      charset.chars.foldLeft((Vector.empty[Char], false)) { case ((acc, flag), c) =>
        if flag then (acc :+ c, true)
        else if c == ch then (acc, true)
        else (acc :+ c, false)
      }._1

  private[this] def calculateStep(ch: Char, consecutive: Consecutive, consecutiveAcc: ConsecutiveAccumulation)
  : Option[Int] =
    if ch.isDigit && consecutiveAcc.current.isDigit then
      Some(ch - consecutiveAcc.current)
    else if ch.isLetter && consecutiveAcc.current.isLetter then
      if consecutive.caseSensitive then Some(ch - consecutiveAcc.current)
      else Some(ch.toLower - consecutiveAcc.current.toLower)
    else None

  private[this] def isConsecutive(ch: Char, consecutive: Consecutive, consecutiveAcc: ConsecutiveAccumulation): Boolean =
    if ch.isLetterOrDigit then
      calculateStep(ch, consecutive, consecutiveAcc) match
        case None => false
        case Some(step) => consecutiveAcc.step match
          case Some(accStep) => accStep == step
          case _ => step.abs <= consecutive.step
    else false

  private[this] def nextConsecutiveAccumulation(ch: Char, consecutiveAccOption: Option[ConsecutiveAccumulation])
  : Option[ConsecutiveAccumulation] =
    if ch.isLetterOrDigit then
      (consecutiveOption, consecutiveAccOption) match
        case (Some(consecutive), Some(consecutiveAcc)) =>
          (calculateStep(ch, consecutive, consecutiveAcc), consecutiveAcc.step) match
            case (Some(step), Some(accStep)) if step.abs <= consecutive.step =>
              if accStep == step then Some(ConsecutiveAccumulation(ch, consecutiveAcc.length + 1, Some(step)))
              else Some(ConsecutiveAccumulation(ch, 2, Some(step)))
            case _ => Some(ConsecutiveAccumulation(ch, 1, None))
        case (Some(_), None) => Some(ConsecutiveAccumulation(ch, 1, None))
        case (None, _) => None
    else None
  end nextConsecutiveAccumulation

  private[this] def checkLength(length: Interval[Int], label: String): Either[Error, Interval[Int]] =
    intervalNonEmpty(length, label).left.map(length *: _)

  private[this] def checkBounded(length: Interval[Int]): Either[Error, Interval[Int]] =
    length.upperBound match
      case _: ValueBound[_] => length.asRight[Error]
      case _ => (length *: UnboundError("upperBound")).asLeft[Interval[Int]]

  private[this] def checkCharset(charset: Charset[C]): Either[Error, Charset[Vector[Char]]] = {
    for
      chars <- nonEmpty(charset.chars, "chars")
      length <-
        if charset.repeat then checkLength(charset.length & Interval.atOrAbove(0), "length")
        else checkLength(charset.length & Interval.closed(0, chars.size), "length")
      length <- if length.isAt(0) then IntervalEmptyError("length").asLeft[Interval[Int]] else length.asRight[Error]
    yield
      charset.copy(chars = chars.toVector, length = length)
  }.left.map(charset *: _)

  private[this] def checkConsecutive: Either[Error, Option[Consecutive]] =
    consecutiveOption.fold(consecutiveOption.asRight[Error]) { consecutive =>
      for
        _ <- positive(consecutive.max, "max")
        _ <- nonNegative(consecutive.step, "step")
      yield consecutiveOption
    }
end CharsetsGen
