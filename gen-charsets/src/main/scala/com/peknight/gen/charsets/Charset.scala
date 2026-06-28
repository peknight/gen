package com.peknight.gen.charsets

import spire.math.Interval

case class Charset(
                    // 字符集
                    chars: String,
                    // 此字符集长度区间
                    length: Interval[Int] = Interval.atOrAbove(0),
                    // 字符是否可重复
                    repeat: Boolean = true,
                    // 此字符集字符是否可用于生成的字符串开头
                    startsWith: Boolean = true,
                    // 此字符集字符是否可用于生成的字符串结尾
                    endsWith: Boolean = true
                  )
