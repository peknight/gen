package com.peknight.gen.charsets

import cats.Monad
import com.peknight.random.Random
import com.peknight.error.std.Error
import spire.math.Interval

case class Charsets[C <: Iterable[Char]](
                                          // 生成使用的字符集
                                          charsets: List[Charset[C]],
                                          // 生成长度区间
                                          length: Interval[Int],
                                          // 连续性限制
                                          consecutiveOption: Option[Consecutive] = None,
                                          // 生成失败重试次数
                                          retry: Int = 3
                                        ):
  def apply[F[_]: Monad](random: Random[F]): F[Either[Error, String]] =
    CharsetsOps.generate[F, C](this).runA(random)
