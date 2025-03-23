package com.peknight.gen.charsets

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.peknight.random.monaderror.Random
import com.peknight.random.provider.RandomProvider
import org.scalatest.flatspec.AsyncFlatSpec

class CharsetsFlatSpec extends AsyncFlatSpec with AsyncIOSpec:
  "Charsets" should "succeed with default" in {
    val run =
      for
        provider <- RandomProvider.of[IO](Random(_))
        given RandomProvider[IO] = provider
        result <- Charsets.default[IO]
        _ <- IO.println(result)
      yield
        result
    run.asserting(either => assert(either.isRight))
  }
end CharsetsFlatSpec
