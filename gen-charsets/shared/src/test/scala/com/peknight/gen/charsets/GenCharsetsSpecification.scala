package com.peknight.gen.charsets

import cats.Id
import cats.syntax.traverse.*
import com.peknight.cats.instances.scalacheck.gen.given
import com.peknight.error.std.Error
import com.peknight.random.Random
import com.peknight.random.id.Random as IdRandom
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Properties}

class GenCharsetsSpecification extends Properties("GenCharsets"):

  val charsetsRandomsGen: Gen[(Charsets[Iterable[Char]], List[Random[Id]])] =
    for
      charsets <- Gen.long.map(seed => CharsetsGen[Id].runA(IdRandom(seed)))
      randoms <- List.fill(100)(Gen.long).traverse(_.map(IdRandom.apply))
    yield (charsets, randoms)

  def review(seed: String): Unit =
    val (charsets, randoms) = charsetsRandomsGen.pureApply(Gen.Parameters.default, Seed.fromBase64(seed).get)
    randoms.foreach { random =>
      charsets(random) match
        case Right(_) => ()
        case result =>
          log(charsets, random, result)
          ()
    }

  def log(charsets: Charsets[Iterable[Char]], random: Random[Id], result: Either[Error, String]): Unit =
    println("================Start================")
    println(s"charsets=$charsets")
    println(s"random=$random")
    println(s"result=$result")
    println("================ End ================")

  // review("fYmGEbGxtRr0ZT2lPepozxNmcLFnKcm_6nH956CeE3N=")
  import org.scalacheck.Prop.forAll
  property("should generate a random string") = forAll (charsetsRandomsGen) { case (charsets, randoms) =>
    randoms.forall { random =>
      charsets(random) match
        case Right(_) => true
        case result =>
          log(charsets, random, result)
          false
    }
  }

  property("should generate a sized string") = forAll(charsetsRandomsGen) { case (charsets, randoms) =>
    randoms.forall { random =>
      charsets(random) match
        case Right(str) => charsets.length.contains(str.length)
        case _ => true
    }
  }
end GenCharsetsSpecification
