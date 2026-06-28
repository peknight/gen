package com.peknight.gen.charsets

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.peknight.random.monaderror.Random
import com.peknight.random.provider.RandomProvider
import org.scalatest.flatspec.AsyncFlatSpec
import spire.math.Interval

class CharsetsFlatSpec extends AsyncFlatSpec with AsyncIOSpec:
  "Charsets" should "pass" in {
    val run =
      for
        provider <- RandomProvider.of[IO](Random(_))
        given RandomProvider[IO] = provider
        result <- Charsets(
          List(
            Charset((0 to 9).mkString, Interval.atOrAbove(3)),
            Charset(('a' to 'z').mkString, Interval.atOrAbove(3)),
            Charset(('A' to 'Z').mkString, Interval.atOrAbove(3)),
            Charset("~!_@.#*$^&", Interval.atOrAbove(3))
          ),
          Interval.point(16),
          Some(Consecutive(3, 3)))[IO]
        _ <- IO.println(result.getOrElse(""))
      yield
        result
    run.asserting(either => assert(either.isRight))
  }
end CharsetsFlatSpec
