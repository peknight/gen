package com.peknight.gen

import cats.data.StateT.pure
import cats.{Applicative, FlatMap, Functor}

import scala.collection.immutable.TreeMap
object Gen:

  def choose[F[_], A](minInclusive: A, maxExclusive: A)(using c: Choose[F, A]): Gen[F, A] =
    c.choose(minInclusive, maxExclusive)

  def oneOf[F[_]: Applicative, A](xs: Iterable[A]): Gen[F, A] =
    if xs.isEmpty then
      throw new IllegalArgumentException("oneOf called on empty collection")
    else
      val vector = xs.toVector
      choose(0, vector.size).map(vector.apply)

  def oneOf[F[_]: Applicative, A](xs: Seq[A]): Gen[F, A] = oneOf(xs: Iterable[A])

  def oneOf[F[_]: Applicative, A](a0: A, a1: A, an: A*): Gen[F, A] = oneOf(a0 +: a1 +: an)

  def oneOf[F[_]: Applicative: FlatMap, A](g0: Gen[F, A], g1: Gen[F, A], gn: Gen[F, A]*): Gen[F, A] =
    val gs = g0 +: g1 +: gn
    choose(0, gs.size).flatMap(gs.apply)

  def option[F[_]: Applicative: FlatMap, A](g: Gen[F, A]): Gen[F, Option[A]] =
    frequency(1 -> pure(None), 9 -> some(g)(using Applicative[F]))

  def some[F[_]: Functor, A](g: Gen[F, A]): Gen[F, Option[A]] = g.map(Some.apply)

  def either[F[_]: Applicative: FlatMap, T, U](gt: Gen[F, T], gu: Gen[F, U]): Gen[F, Either[T, U]] =
    oneOf(gt.map(Left.apply)(using Applicative[F]), gu.map(Right.apply)(using Applicative[F]))

  def frequency[F[_]: Applicative: FlatMap, A](gs: (Int, Gen[F, A])*): Gen[F, A] =
    val filtered = gs.iterator.filter(_._1 > 0).toVector
    if filtered.isEmpty then
      throw new IllegalArgumentException("no items with positive weights")
    else
      val (total, treeMap) = filtered.foldLeft((0L, TreeMap.empty[Long, Gen[F, A]])) {
        case ((total, treeMap), (weight, value)) =>
          val next = total + weight
          (next, treeMap + (next -> value))
      }
      choose(1L, total).flatMap(r => treeMap.rangeFrom(r).head._2)

end Gen
