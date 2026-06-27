package com.peknight

import cats.data.StateT
import com.peknight.random.Random

package object gen:
  type Gen[F[_], A] = StateT[F, Random[F], A]
end gen
