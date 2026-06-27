package com.peknight.gen

import cats.Id
import cats.data.StateT
import com.peknight.random.Random

package object id:
  type Gen[A] = StateT[Id, Random[Id], A]
  type Choose[A] = com.peknight.gen.Choose[Id, A]
end id
