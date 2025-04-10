package com.peknight.gen.charsets

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.peknight.random.monaderror.Random
import com.peknight.random.provider.RandomProvider
import org.scalatest.flatspec.AsyncFlatSpec
import spire.math.Interval

class CharsetsFlatSpec extends AsyncFlatSpec with AsyncIOSpec:
  "Charsets" should "succeed with default" in {
    val run =
      for
        provider <- RandomProvider.of[IO](Random(_))
        given RandomProvider[IO] = provider
        result <- Charsets(
          List(
            Charset((0 to 9).mkString, Interval.atOrAbove(2)),
            Charset(('a' to 'z').mkString, Interval.atOrAbove(2)),
            Charset(('A' to 'Z').mkString, Interval.atOrAbove(2)),
            Charset("~!_@.#*$^&", Interval.atOrAbove(2))
          ),
          Interval.point(8),
          Some(Consecutive(3, 3)))[IO]
        _ <- IO.println(result)
      yield
        result
    run.asserting(either => assert(either.isRight))
  }
end CharsetsFlatSpec
