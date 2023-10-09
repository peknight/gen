package com.peknight.gen

import cats.Applicative

trait ChooseT[F[_], A] extends Serializable:
  def choose(minInclusive: A, maxExclusive: A): GenT[F, A]
end ChooseT
object ChooseT extends ChooseInstances:

  def xmap[F[_] : Applicative, T, U](from: T => U, to: U => T)(using c: ChooseT[F, T]): ChooseT[F, U] =
    (minInclusive: U, maxExclusive: U) => c.choose(to(minInclusive), to(maxExclusive)).map(from)
end ChooseT

