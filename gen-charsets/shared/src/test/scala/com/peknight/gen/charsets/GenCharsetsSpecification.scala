package com.peknight.gen.charsets

import cats.Id
import cats.syntax.traverse.*
import com.peknight.cats.instances.scalacheck.gen.given
import com.peknight.error.std.Error
import com.peknight.random.Random
import com.peknight.random.id.Random as IdRandom
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Properties}

class GenCharsetsSpecification extends Properties("CharsetsGen"):

  val charsetsRandomsGen: Gen[(Charsets[Iterable[Char]], List[Random[Id]])] =
    for
      charsets <- Gen.long.map(seed => CharsetsGen.charsetsGen[Id].runA(IdRandom(seed)))
      randoms <- List.fill(100)(Gen.long).traverse(_.map(IdRandom.apply))
    yield (charsets, randoms)

  def review(seed: String): Unit =
    val (charsets, randoms) = charsetsRandomsGen.pureApply(Gen.Parameters.default, Seed.fromBase64(seed).get)
    randoms.foreach { random =>
      GenCharsets[Id, Iterable[Char]](charsets)(random) match
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

  review("4Ield9xMDlqSvTfnSiCjApMvXm-u0m9jxUG6vkfHapH=")
  // import org.scalacheck.Prop.forAll
  // property("should generate a random string") = forAll (charsetsRandomsGen) { case (charsets, randoms) =>
  //   randoms.forall { random =>
  //     GenCharsets[Id, Iterable[Char]](charsets)(random) match
  //       case Right(_) => true
  //       case result =>
  //         log(charsets, random, result)
  //         false
  //   }
  // }
end GenCharsetsSpecification
