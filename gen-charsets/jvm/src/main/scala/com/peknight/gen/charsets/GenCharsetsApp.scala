package com.peknight.gen.charsets

import cats.Id
import com.peknight.random.id.Random as IdRandom
import spire.math.Interval

object GenCharsetsApp extends App:
  println(Charsets(
    List(
      Charset((0 to 9).mkString, Interval.atOrAbove(3)),
      Charset(('a' to 'z').mkString, Interval.atOrAbove(3)),
      Charset(('A' to 'Z').mkString, Interval.atOrAbove(3)),
      Charset("~!_@.#*$^&", Interval.atOrAbove(3))
    ),
    Interval.point(16),
    Some(Consecutive(3,3))
  )(IdRandom(System.currentTimeMillis())))
  println(CharsetsOps.allocate[Id, Int](
    Interval.closed(30, 40),
    Map(
      1 -> Interval.above(10),
      2 -> Interval.below(10),
      3 -> Interval.closed(8, 10),
      4 -> Interval.all[Int]
    )
  ).map(_.runA(IdRandom(System.currentTimeMillis()))))
end GenCharsetsApp
