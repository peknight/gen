package com.peknight.gen.charsets

import com.peknight.random.id.Random as IdRandom
import spire.math.Interval

object GenCharsetsApp extends App:
  println(CharsetsGen(
    List(
      Charset((0 to 9).mkString, Interval.atOrAbove(3)),
      Charset(('a' to 'z').mkString, Interval.atOrAbove(3)),
      Charset(('A' to 'Z').mkString, Interval.atOrAbove(3)),
      Charset("~!_@.#*$^&", Interval.atOrAbove(3))
    ),
    Interval.point(16),
    Some(Consecutive(3,3))
  )(IdRandom(System.currentTimeMillis())))
end GenCharsetsApp
