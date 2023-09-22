package com.peknight.gen.charsets.accumulation

import com.peknight.gen.charsets.interval.LengthInterval

case class CharsetAccumulation(chars: Vector[Char], length: LengthInterval, repeat: Boolean, startsWith: Boolean,
                               endsWith: Boolean)
