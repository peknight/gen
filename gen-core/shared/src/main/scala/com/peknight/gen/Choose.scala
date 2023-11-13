package com.peknight.gen

import cats.Applicative
import com.peknight.gen.instances.ChooseInstances

trait Choose[F[_], A] extends Serializable:
  def choose(minInclusive: A, maxExclusive: A): Gen[F, A]
end Choose
object Choose extends ChooseInstances:
  def xmap[F[_] : Applicative, T, U](from: T => U, to: U => T)(using c: Choose[F, T]): Choose[F, U] =
    (minInclusive: U, maxExclusive: U) => c.choose(to(minInclusive), to(maxExclusive)).map(from)
end Choose

