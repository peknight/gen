package com.peknight.gen.charsets

import spire.math.Interval

object CharsetGenInstances:
  private[charsets] val elementLength = Interval.atOrAbove(3)
  private[charsets] val globalLength = Interval.closed(12, 20)
  private[charsets] val digitLength = elementLength
  private[charsets] val lowerLength = elementLength
  private[charsets] val upperLength = elementLength
  private[charsets] val specialLength = elementLength
  private[charsets] val specialChars = "~!_@.#*$^&"
  private[charsets] val gen = CharsetsGen(
    List(
      Charset((0 to 9).mkString, digitLength),
      Charset(('a' to 'z').mkString, lowerLength),
      Charset(('A' to 'Z').mkString, upperLength),
      Charset(specialChars, specialLength, false, false, false)
    ),
    globalLength,
    Some(Consecutive(3, 3)),
    16
  )
end CharsetGenInstances
