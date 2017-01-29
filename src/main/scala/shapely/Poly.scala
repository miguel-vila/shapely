package shapely

trait Poly { self =>

  final def at[A] = new {
    def apply[B](f: A => B): Case[A,B] = new Case[A,B] {
      def apply(a: A) = f(a)
    }
  }

  sealed trait Case[A,B] {
    def apply(a: A): B
  }

  def apply[A,B](a: A)(implicit C: this.Case[A,B]): B = C(a)

  def andThen(other: Poly): Poly = new Poly {
    import self._
    import other._
    implicit def andThenCases[A,B,C](implicit caseAB: self.Case[A,B], caseBC: other.Case[B,C]): this.Case[A,C] =
      new this.Case[A,C] {
        def apply(a: A): C = caseBC(caseAB(a))
      }
  }

}


object Poly {

}
