/**
  * Created by matt.fenwick on 9/8/16.
  */

sealed trait MaybeError[+E, +A] {
  def fmap[B](f: A => B): MaybeError[E, B] = this match {
    case Zero => Zero
    case Error(e) => Error(e)
    case Success(x) => Success(f(x))
  }
  // nope: def bind[B](f: A => MaybeError[E, B]): MaybeError[E, B] = ...
  def bind[B, E2 >: E](f: A => MaybeError[E2, B]): MaybeError[E2, B] = this match {
    case Zero => Zero
    case Error(e) => Error(e)
    case Success(x) => f(x)
  }
  def plus[A2 >: A, E2 >: E](that: MaybeError[E2, A2]): MaybeError[E2, A2] = this match {
    case Zero => that
    case default => default
  }
  def mapError[E2](f: E => E2): MaybeError[E2, A] = this match {
    case Zero => Zero
    case Error(e) => Error(f(e))
    case Success(x) => Success(x)
  }
}

case object Zero extends MaybeError[Nothing, Nothing]
case class Error[E](value: E) extends MaybeError[E, Nothing]
case class Success[A](value: A) extends MaybeError[Nothing, A]

object MaybeError {
  def app2[E, A, B, C](f: (A, B) => C, a: MaybeError[E, A], b: MaybeError[E, B]): MaybeError[E, C] = {
    a.bind(x =>
      b.bind(y =>
        Success(f(x, y))))
  }
}
