package com.peknight.gen

import cats.Applicative
import cats.data.StateT.pure
import com.peknight.random.state.*

import scala.concurrent.duration.{Duration, FiniteDuration}

trait ChooseInstances2:

  protected[gen] class IllegalBoundsError[A](minInclusive: A, maxExclusive: A)
    extends IllegalArgumentException(s"invalid bounds: minInclusive=$minInclusive, maxExclusive=$maxExclusive")

  given [F[_]: Applicative]: ChooseT[F, Long] with
    def choose(minInclusive: Long, maxExclusive: Long): GenT[F, Long] =
      if maxExclusive <= minInclusive then throw new IllegalBoundsError(minInclusive, maxExclusive)
      else if maxExclusive == minInclusive + 1 then pure(minInclusive)
      else if minInclusive == Long.MinValue && maxExclusive == Long.MaxValue then nextLong
      else if minInclusive == Int.MinValue && maxExclusive == Int.MaxValue then nextInt.map(_.toLong)
      else if minInclusive == Short.MinValue && maxExclusive == Short.MaxValue then nextInt.map(_.toShort.toLong)
      else if minInclusive == Char.MinValue && maxExclusive == Char.MaxValue then nextInt.map(_.toChar.toLong)
      else if minInclusive == Byte.MinValue && maxExclusive == Byte.MaxValue then nextInt.map(_.toByte.toLong)
      else between(minInclusive, maxExclusive)
  end given

  given [F[_]: Applicative]: ChooseT[F, Int] with
    def choose(minInclusive: Int, maxExclusive: Int): GenT[F, Int] =
      if maxExclusive <= minInclusive then throw new IllegalBoundsError(minInclusive, maxExclusive)
      else if maxExclusive == minInclusive + 1 then pure(minInclusive)
      else if minInclusive == Int.MinValue && maxExclusive == Int.MaxValue then nextInt
      else if minInclusive == Short.MinValue && maxExclusive == Short.MaxValue then nextInt.map(_.toShort.toInt)
      else if minInclusive == Char.MinValue && maxExclusive == Char.MaxValue then nextInt.map(_.toChar.toInt)
      else if minInclusive == Byte.MinValue && maxExclusive == Byte.MaxValue then nextInt.map(_.toByte.toInt)
      else between(minInclusive, maxExclusive)
  end given

  given [F[_]: Applicative]: ChooseT[F, Short] with
    def choose(minInclusive: Short, maxExclusive: Short): GenT[F, Short] =
      if maxExclusive <= minInclusive then throw new IllegalBoundsError(minInclusive, maxExclusive)
      else if maxExclusive == minInclusive + 1 then pure(minInclusive)
      else if minInclusive == Short.MinValue && maxExclusive == Short.MaxValue then nextInt.map(_.toShort)
      else if minInclusive == Char.MinValue && maxExclusive == Char.MaxValue then nextInt.map(_.toChar.toShort)
      else if minInclusive == Byte.MinValue && maxExclusive == Byte.MaxValue then nextInt.map(_.toByte.toShort)
      else between(minInclusive, maxExclusive).map(_.toShort)
  end given

  given [F[_]: Applicative]: ChooseT[F, Char] with
    def choose(minInclusive: Char, maxExclusive: Char): GenT[F, Char] =
      if maxExclusive <= minInclusive then throw new IllegalBoundsError(minInclusive, maxExclusive)
      else if maxExclusive == minInclusive + 1 then pure(minInclusive)
      else if minInclusive == Char.MinValue && maxExclusive == Char.MaxValue then nextInt.map(_.toChar)
      else if minInclusive == Byte.MinValue && maxExclusive == Byte.MaxValue then nextInt.map(_.toByte.toChar)
      else between(minInclusive, maxExclusive).map(_.toChar)
  end given

  given [F[_]: Applicative]: ChooseT[F, Byte] with
    def choose(minInclusive: Byte, maxExclusive: Byte): GenT[F, Byte] =
      if maxExclusive <= minInclusive then throw new IllegalBoundsError(minInclusive, maxExclusive)
      else if maxExclusive == minInclusive + 1 then pure(minInclusive)
      else if minInclusive == Byte.MinValue && maxExclusive == Byte.MaxValue then nextInt.map(_.toByte)
      else between(minInclusive, maxExclusive).map(_.toByte)
  end given

  given doubleChoose[F[_]: Applicative]: ChooseT[F, Double] with
    def choose(minInclusive: Double, maxExclusive: Double): GenT[F, Double] =
      if maxExclusive <= minInclusive then throw new IllegalBoundsError(minInclusive, maxExclusive)
      else if minInclusive == Double.MinValue && maxExclusive == Double.MaxValue then nextDouble
      else between(minInclusive, maxExclusive)
  end doubleChoose

  given floatChoose[F[_]: Applicative]: ChooseT[F, Float] with
    def choose(minInclusive: Float, maxExclusive: Float): GenT[F, Float] =
      if maxExclusive <= minInclusive then throw new IllegalBoundsError(minInclusive, maxExclusive)
      else if minInclusive == Float.MinValue && maxExclusive == Float.MaxValue then nextFloat
      else between(minInclusive, maxExclusive)
  end floatChoose

  given [F[_]: Applicative]: ChooseT[F, FiniteDuration] =
    ChooseT.xmap[F, Long, FiniteDuration](Duration.fromNanos, _.toNanos)
  end given

end ChooseInstances2
