package com.peknight.gen.charsets

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
                                        )
