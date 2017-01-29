package shapely

trait HList {
  type Append[L <: HList] <: HList

  def ++[L <: HList](xs: L): Append[L]

}

case class HCons[H, T <: HList](head: H, tail: T) extends HList {
  type Append[L <: HList] = HCons[H, T#Append[L]]

  def ++[L <: HList](xs: L) = HCons(head, tail ++ xs)
}

case object HNil0 extends HList {
  type Append[L <: HList] = L

  def ++[L <: HList](xs: L) = xs
}

trait Remover[A, L <: HList] {

  type Out <: HList

  def apply(xs: L): Out

}

trait Mapper[ L <: HList, P <: Poly ] {

  type Out <: HList

  def apply(xs: L): Out

}

object Mapper {

  type Aux[L <: HList, P <: Poly, Out0 <: HList] = Mapper[L, P] { type Out = Out0 }

  implicit def base[P <: Poly]: Mapper.Aux[HNil, P, HNil] = new Mapper[HNil, P] {
    type Out = HNil
    def apply(xs: HNil) = HNil
  }

  implicit def cons[A, B, L <: HList, P <: Poly](implicit f: P#Case[A,B], M: Mapper[L, P]): Mapper.Aux[A :: L, P, B :: M.Out] =
    new Mapper[A :: L, P] {
      type Out = B :: M.Out
      def apply(xs: A :: L) = f(xs.head) :: M(xs.tail)
    }

}

trait RemoverLowPriorityImplicits {

  // This is really the base case when the tail is empty
  implicit def base[A]: Remover.Aux[A, A :: HNil, HNil] =
    new Remover[A, A :: HNil] {
      type Out = HNil

      def apply(xs: A :: HNil) = xs.tail
    }

}

object Remover extends RemoverLowPriorityImplicits {

  type Aux[A, L <: HList, Out0 <: HList ] = Remover[A,L] { type Out = Out0 }

  implicit def corecurseRemove[A, L <: HList](implicit R: Remover[A,L]): Remover.Aux[A, A :: L, R.Out] =
    new Remover[A, A::L] {
      type Out = R.Out

      def apply(xs: A :: L) = R(xs.tail)
    }

  // This is failing because the compiler can't tell the difference between this case and
  // previous one. This case should state explicitly that `A` and `B` are different.
  implicit def otherCase[A, B, L <: HList](implicit R: Remover[A, L]): Remover.Aux[A, B :: L, B :: R.Out] =
    new Remover[A, B :: L] {
      type Out = B :: R.Out

      def apply(xs: B :: L) = xs.head :: R(xs.tail)
    }

}
