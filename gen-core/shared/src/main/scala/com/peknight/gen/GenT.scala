package com.peknight.gen

import cats.data.StateT.pure
import cats.{Applicative, FlatMap, Functor}

import scala.collection.immutable.TreeMap
object GenT:

  def choose[F[_], A](minInclusive: A, maxExclusive: A)(using c: ChooseT[F, A]): GenT[F, A] =
    c.choose(minInclusive, maxExclusive)

  def oneOf[F[_]: Applicative, A](xs: Iterable[A]): GenT[F, A] =
    if xs.isEmpty then
      throw new IllegalArgumentException("oneOf called on empty collection")
    else
      val vector = xs.toVector
      choose(0, vector.size).map(vector.apply)

  def oneOf[F[_]: Applicative, A](xs: Seq[A]): GenT[F, A] = oneOf(xs: Iterable[A])

  def oneOf[F[_]: Applicative, A](a0: A, a1: A, an: A*): GenT[F, A] = oneOf(a0 +: a1 +: an)

  def oneOf[F[_]: Applicative: FlatMap, A](g0: GenT[F, A], g1: GenT[F, A], gn: GenT[F, A]*): GenT[F, A] =
    val gs = g0 +: g1 +: gn
    choose(0, gs.size).flatMap(gs.apply)

  def option[F[_]: Applicative: FlatMap, A](g: GenT[F, A]): GenT[F, Option[A]] =
    frequency(1 -> pure(None), 9 -> some(g)(using Applicative[F]))

  def some[F[_]: Functor, A](g: GenT[F, A]): GenT[F, Option[A]] = g.map(Some.apply)

  def either[F[_]: Applicative: FlatMap, T, U](gt: GenT[F, T], gu: GenT[F, U]): GenT[F, Either[T, U]] =
    oneOf(gt.map(Left.apply)(using Applicative[F]), gu.map(Right.apply)(using Applicative[F]))

  def frequency[F[_]: Applicative: FlatMap, A](gs: (Int, GenT[F, A])*): GenT[F, A] =
    val filtered = gs.iterator.filter(_._1 > 0).toVector
    if filtered.isEmpty then
      throw new IllegalArgumentException("no items with positive weights")
    else
      val (total, treeMap) = filtered.foldLeft((0L, TreeMap.empty[Long, GenT[F, A]])) {
        case ((total, treeMap), (weight, value)) =>
          val next = total + weight
          (next, treeMap + (next -> value))
      }
      choose(1L, total).flatMap(r => treeMap.rangeFrom(r).head._2)

end GenT
