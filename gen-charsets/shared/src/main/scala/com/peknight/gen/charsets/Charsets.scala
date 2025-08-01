package com.peknight.gen.charsets

import cats.data.EitherT
import cats.{Monad, MonadError}
import com.peknight.error.Error
import com.peknight.error.syntax.applicativeError.faeLiftET
import com.peknight.random.Random
import com.peknight.random.provider.RandomProvider
import spire.math.Interval

case class Charsets(
                     // 生成使用的字符集
                     charsets: List[Charset],
                     // 生成长度区间
                     length: Interval[Int],
                     // 连续性限制
                     consecutiveOption: Option[Consecutive] = None,
                     // 生成失败重试次数
                     retry: Int = 3
                   ):
  def random[F[_]: Monad](random: Random[F]): F[Either[Error, String]] = CharsetsOps.generate[F](this).runA(random)
  def apply[F[_]](using MonadError[F, Throwable], RandomProvider[F]): F[Either[Error, String]] =
    val eitherT =
      for
        rand <- RandomProvider[F].random.faeLiftET
        result <- EitherT(random[F](rand))
      yield
        result
    eitherT.value
object Charsets:
  val default: Charsets = Charsets(
    List(
      Charset((0 to 9).mkString, Interval.atOrAbove(3)),
      Charset(('a' to 'z').mkString, Interval.atOrAbove(3)),
      Charset(('A' to 'Z').mkString, Interval.atOrAbove(3)),
      Charset("~!_@.#*$^&", Interval.atOrAbove(3))
    ),
    Interval.point(16),
    Some(Consecutive(3, 3))
  )
end Charsets
