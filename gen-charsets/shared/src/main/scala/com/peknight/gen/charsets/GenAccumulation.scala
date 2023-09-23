package com.peknight.gen.charsets

case class GenAccumulation(chars: List[Char], charsets: Map[Int, Charset[Vector[Char]]], remain: Int,
                           consecutiveAccumulation: Option[ConsecutiveAccumulation])
