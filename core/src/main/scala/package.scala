package object fun {
  type @@[T, Tag] = Tagged[T, Tag]
  private[fun] type Tagged[A, T] = {type Tag = T; type Self = A}
  type ~>[-F[_],+G[_]] = NaturalTransformation[F,G]
  type <~>[F[_],G[_]] = Isomorphism.IsoFunctor[F,G]
  type <=>[A,B] = Isomorphism.IsoSet[A,B]
}
