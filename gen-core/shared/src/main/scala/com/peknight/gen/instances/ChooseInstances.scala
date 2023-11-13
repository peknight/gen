package com.peknight.gen.instances

import cats.data.StateT.pure
import cats.{Applicative, FlatMap}
import com.peknight.gen.{Choose, Gen}
import com.peknight.random.state.{between, nextDouble, nextFloat}
trait ChooseInstances extends ChooseInstances2:

  given doubleChooseFlatMap[F[_]: Applicative: FlatMap]: Choose[F, Double] with
    def choose(minInclusive: Double, maxExclusive: Double): Gen[F, Double] =
      if maxExclusive <= minInclusive then throw new IllegalBoundsError(minInclusive, maxExclusive)
      else if minInclusive == Double.MinValue && maxExclusive == Double.MaxValue then nextDouble
      else if minInclusive == Double.NegativeInfinity && maxExclusive == Double.PositiveInfinity then
        Gen.frequency(1 -> pure(Double.NegativeInfinity), 1 -> pure(Double.PositiveInfinity),
          8 -> choose(Double.MinValue, maxExclusive))
      else if minInclusive == Double.NegativeInfinity then
        Gen.frequency(1 -> pure(Double.NegativeInfinity), 9 -> choose(Double.MinValue, maxExclusive))
      else if maxExclusive == Double.PositiveInfinity then
        Gen.frequency(1 -> pure(Double.PositiveInfinity), 9 -> choose(minInclusive, Double.MaxValue))
      else between(minInclusive, maxExclusive)
  end doubleChooseFlatMap

  given floatChooseFlatMap[F[_]: Applicative: FlatMap]: Choose[F, Float] with
    def choose(minInclusive: Float, maxExclusive: Float): Gen[F, Float] =
      if maxExclusive <= minInclusive then throw new IllegalBoundsError(minInclusive, maxExclusive)
      else if minInclusive == Float.MinValue && maxExclusive == Float.MaxValue then nextFloat
      else if minInclusive == Float.NegativeInfinity && maxExclusive == Float.PositiveInfinity then
        Gen.frequency(1 -> pure(Float.NegativeInfinity), 1 -> pure(Float.PositiveInfinity),
          8 -> choose(Float.MinValue, maxExclusive))
      else if minInclusive == Float.NegativeInfinity then
        Gen.frequency(1 -> pure(Float.NegativeInfinity), 9 -> choose(Float.MinValue, maxExclusive))
      else if maxExclusive == Float.PositiveInfinity then
        Gen.frequency(1 -> pure(Float.PositiveInfinity), 9 -> choose(minInclusive, Float.MaxValue))
      else between(minInclusive, maxExclusive)
  end floatChooseFlatMap
end ChooseInstances
