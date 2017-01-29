package shapely

object Example {

  val l1 = 1 :: 2.0 :: "abc" :: 3 :: HNil

  import Remover._

  //val l2  = l1.remove[Int]
  val l3 : Int :: Int :: HNil = 1 :: 2 :: HNil0
  val l4 = l3.remove[Int]

  object square extends Poly {
    implicit val is = at[Int] { i => i * i }
    implicit val fs = at[Float] { f => f * f }
    implicit val ds = at[Double] { d => d * d }
  }

  object addOne extends Poly {
    implicit val ia1: Case[Int,Int] = at[Int] { i => i + 1 }
    implicit val da1 = at[Double] { d => d + 1 }
    implicit val sa1 = at[String] { s => (s + 1).toString }

  }

  square(2)
  square(2.0f)
  square(2.0d)

  val comp = square andThen addOne

  val x: Int = comp[Int,Int](3)

  object transform extends Poly {
    implicit val is = at[Int] { _ + 3 }
    implicit val bs = at[Boolean] { !_ }
    implicit val ss = at[String] { _ + " there!" }
  }

  val xs = 1 :: true :: "hi" :: HNil

  import Mapper._

  println(xs.map(transform))

}
