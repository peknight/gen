package com.peknight.gen.charsets

import cats.Id
import cats.syntax.either.*
import cats.syntax.traverse.*
import com.peknight.cats.instances.scalacheck.gen.given
import com.peknight.error.Error
import com.peknight.random.Random
import com.peknight.random.id.Random as IdRandom
import org.scalacheck.Prop.forAll
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Properties}

class CharsetsSpecification extends Properties("Charsets"):

  val charsetsRandomsGen: Gen[(Charsets[Iterable[Char]], List[Random[Id]])] =
    for
      charsets <- Gen.long.map(seed => CharsetsGen[Id].runA(IdRandom(seed)))
      randoms <- List.fill(100)(Gen.long).traverse(_.map(IdRandom.apply))
    yield (charsets, randoms)

  def review(seed: String): (Charsets[Iterable[Char]], List[(Random[Id], Either[Error, String])]) =
    val (charsets, randoms) = charsetsRandomsGen.pureApply(Gen.Parameters.default, Seed.fromBase64(seed).get)
    (charsets, randoms.map(random => (random, charsets(random))))

  println(review("fYmGEbGxtRr0ZT2lPepozxNmcLFnKcm_6nH956CeE3N="))

  property("should generate a random string") = forAll (charsetsRandomsGen) { case (charsets, randoms) =>
    randoms.forall { random =>
      charsets(random) match
        case Right(_) => true
        case _ => false
    }
  }

  property("should generate a string with bounded length") = forAll(charsetsRandomsGen) { case (charsets, randoms) =>
    randoms.forall { random =>
      charsets(random) match
        case Right(str) => charsets.length.contains(str.length)
        case _ => true
    }
  }
end CharsetsSpecification
