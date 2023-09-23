package com.peknight.gen.charsets

import com.peknight.random.id.Random as IdRandom
import spire.math.Interval

object GenCharsetsApp extends App:
  println(CharsetsGen(
    List(
      Charset((0 to 9).mkString, Interval.atOrAbove(1), false),
      Charset(('a' to 'z').mkString, Interval.atOrAbove(1), false),
      Charset(('A' to 'Z').mkString, Interval.atOrAbove(1), false),
      Charset("~!_@.#*$^&", Interval.atOrAbove(1), false)
    ),
    Interval.closed(12, 20),
    Some(Consecutive(3,3))
  )(IdRandom(System.currentTimeMillis())))
end GenCharsetsApp
