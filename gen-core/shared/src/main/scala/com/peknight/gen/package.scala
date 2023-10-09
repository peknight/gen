package com.peknight

import cats.Id
import cats.data.StateT
import com.peknight.random.Random

package object gen:
  type GenT[F[_], A] = StateT[F, Random[F], A]
  type Gen[A] = StateT[Id, Random[Id], A]
  type Choose[A] = ChooseT[Id, A]
end gen
