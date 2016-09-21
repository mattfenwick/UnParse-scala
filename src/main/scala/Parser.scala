import scala.collection.mutable

/**
  * Created by matt.fenwick on 9/8/16.
  */

// TODO `var` instead of `val` in order to allow mutual recursion
class Parser[E, S, T, A](var parse: (List[T], S) => MaybeError[E, (List[T], S, A)]) {
  def <*>[B, C](implicit ev: A =:= (B => C), p: Parser[E, S, T, B]): Parser[E, S, T, C] = {
    // TODO this is garbage and doesn't work.  fix it
    import Parser.app
    // TODO this is really awful, there's got to be a better way to do this
    val f = this.parse.asInstanceOf[(List[T], S) => MaybeError[E, (List[T], S, B => C)]]
    app(new Parser(f), p)
  }
}

private object Helpers {
  def good[E, S, T, A](rest: List[T], state: S, value: A): MaybeError[E, (List[T], S, A)] = {
    Success((rest, state, value))
  }
}

object Parser {
  def fmap[E, S, T, A, B](g: A => B, parser: Parser[E, S, T, A]): Parser[E, S, T, B] = {
    new Parser((xs, s) => parser.parse(xs, s).fmap(r => (r._1, r._2, g(r._3))))
  }

  implicit def pure[E, S, T, A](x: A): Parser[E, S, T, A] = {
    new Parser((xs, s) => Helpers.good(xs, s, x))
  }

  def bind[E, S, T, A, B](parser: Parser[E, S, T, A], g: A => Parser[E, S, T, B]): Parser[E, S, T, B] = {
    new Parser((xs, s) => parser.parse(xs, s) match {
      case Zero => Zero
      case Error(e) => Error(e)
      case Success(r) => g(r._3).parse(r._1, r._2)
    })
  }

  def error[E, S, T, A](e: E): Parser[E, S, T, A] = {
    new Parser((_, _) => Error(e))
  }

  def catchError[E, E2, S, T, A](f: E => Parser[E2, S, T, A], parser: Parser[E, S, T, A]): Parser[E2, S, T, A] = {
    new Parser((xs, s) => parser.parse(xs, s) match {
      case Zero => Zero
      case Error(e) => f(e).parse(xs, s)
      case Success(r) => Success(r)
    })
  }

  def mapError[E, E2, S, T, A](f: E => E2, parser: Parser[E, S, T, A]): Parser[E2, S, T, A] = {
    catchError(error[E2, S, T, A] _ compose f, parser)
  }

  def put[E, S, T](xs: List[T]): Parser[E, S, T, Unit] = {
    new Parser((_, s) => Helpers.good(xs, s, Unit))
  }

  def putState[E, S, T](s: S): Parser[E, S, T, Unit] = {
    new Parser((xs, _) => Helpers.good(xs, s, Unit))
  }

  def updateState[E, S, T](g: S => S): Parser[E, S, T, Unit] = {
    new Parser((xs, s) => Helpers.good(xs, g(s), Unit))
  }

  def check[E, S, T, A](predicate: A => Boolean, parser: Parser[E, S, T, A]): Parser[E, S, T, A] = {
    new Parser((xs, s) => parser.parse(xs, s) match {
      case Zero => Zero
      case Error(e) => Error(e)
      case Success(x) => if (predicate(x._3)) {
        Success(x)
      } else {
        Zero
      }
    })
  }

  def many0[E, S, T, A](parser: Parser[E, S, T, A]): Parser[E, S, T, List[A]] = {
    // TODO wow, do a better job here
    new Parser((xs: List[T], s: S) => {
      var vals = mutable.MutableList[A]()
      var tokens: List[T] = xs
      var state: S = s
      var out: MaybeError[E, (List[T], S, List[A])] = Zero // TODO ugh why
      var continue = true
      while (continue) {
        //println(s"loop; parser, tokens, state: $parser $tokens $state")
        val r = parser.parse(tokens, state)
        r match {
          case Zero => {
            out = Helpers.good[E, S, T, List[A]](tokens, state, vals.toList)
            continue = false
          }
          case Error(e) => {
            out = Error(e)
            continue = false
          }
          case Success(x) => {
            tokens = x._1
            state = x._2
            vals += x._3
          }
        }
      }
      out
    })
    // doesn't work, because many0(parser) isn't evaluated lazily:
//    alt(List(app2((x: A, xs: List[A]) => x :: xs, parser, many0(parser)), pure(List[A]())))
  }

  def many1[E, S, T, A](parser: Parser[E, S, T, A]): Parser[E, S, T, List[A]] = {
    check((xs: List[A]) => xs match {
      case Nil => false
      case _ => true
    }, many0(parser))
  }

  def seq[E, S, T, A](parsers: List[Parser[E, S, T, A]]): Parser[E, S, T, List[A]] = parsers match {
    case Nil => pure(Nil)
    case (p :: ps) => app(app(pure((x: A) => (xs: List[A]) => x :: xs), p), seq(ps))
  }

  def app[E, S, T, A, B](p1: Parser[E, S, T, A => B], p2: Parser[E, S, T, A]): Parser[E, S, T, B] = {
    bind(p1, (f: (A => B)) =>
      fmap((x: A) => f(x), p2))
  }

  def app2[E, S, T, A, B, C](p1: Parser[E, S, T, A => B => C], p2: Parser[E, S, T, A], p3: Parser[E, S, T, B]): Parser[E, S, T, C] = {
    app(app(p1, p2), p3)
  }

  def app3[E, S, T, A, B, C, D](p1: Parser[E, S, T, A => B => C => D], p2: Parser[E, S, T, A], p3: Parser[E, S, T, B], p4: Parser[E, S, T, C]): Parser[E, S, T, D] = {
    app(app(app(p1, p2), p3), p4)
  }

  def app4[E, S, T, A, B, C, D, W](p1: Parser[E, S, T, A => B => C => D => W],
                                   p2: Parser[E, S, T, A],
                                   p3: Parser[E, S, T, B],
                                   p4: Parser[E, S, T, C],
                                   p5: Parser[E, S, T, D]): Parser[E, S, T, W] = {
    app(app(app(app(p1, p2), p3), p4), p5)
  }

  def seq2L[E, S, T, A, B](p1: Parser[E, S, T, A], p2: Parser[E, S, T, B]): Parser[E, S, T, A] = {
    app2((x: A) => (_: B) => x, p1, p2)
  }

  def seq2R[E, S, T, A, B](p1: Parser[E, S, T, A], p2: Parser[E, S, T, B]): Parser[E, S, T, B] = {
    //pure((_: A) => (y: B) => y) app p1 app p2
    app2((_: A) => (y: B) => y, p1, p2)
  }

  def zero[E, S, T, A](): Parser[E, S, T, A] = {
    new Parser((_, _) => Zero)
  }

  def get[E, S, T](): Parser[E, S, T, List[T]] = {
    new Parser((xs, s) => Helpers.good(xs, s, xs))
  }

  def getState[E, S, T](): Parser[E, S, T, S] = {
    new Parser((xs, s) => Helpers.good(xs, s, s))
  }

  def lookahead[E, S, T, A](parser: Parser[E, S, T, A]): Parser[E, S, T, A] = {
    bind(get(), (xs: List[T]) =>
      bind(getState(), (s: S) =>
        seq2L(parser, seq(List[Parser[E, S, T, Unit]](put(xs), putState(s))))))
  }

  def not0[E, S, T, A](parser: Parser[E, S, T, A]): Parser[E, S, T, Unit] = {
    new Parser((xs, s) => parser.parse(xs, s) match {
      case Zero => Helpers.good(xs, s, Unit)
      case Error(x) => Error(x)
      case Success(_) => Zero
    })
  }

  def alt[E, S, T, A](parsers: List[Parser[E, S, T, A]]): Parser[E, S, T, A] = {
    // TODO this should either be a fold or a for-loop
    new Parser((xs, s) => parsers match {
      case Nil => Zero
      case (p :: ps) => p.parse(xs, s) match {
        case Zero => alt(ps).parse(xs, s)
        case Error(e) => Error(e)
        case Success(x) => Success(x)
      }
    })
  }

  def optional[E, S, T, A](parser: Parser[E, S, T, A]): Parser[E, S, T, Option[A]] = {
    alt(List(fmap((x: A) => Some(x), parser), pure(None)))
  }

  def optionalV[E, S, T, A](parser: Parser[E, S, T, A], defaultValue: A): Parser[E, S, T, A] = {
    alt(List(parser, pure(defaultValue)))
  }

  def commit[E, S, T, A](e: E, parser: Parser[E, S, T, A]): Parser[E, S, T, A] = {
    alt(List(parser, error(e)))
  }

  def _buildSepByValue[A, B] = (first: A) => (pairs: List[(B, A)]) => {
    (first :: pairs.map({ case (_, a) => a }), pairs.map({ case (b, _) => b }))
  }

  def _pair[A, B] = (a: A) => (b: B) => (a, b)

  def sepBy1[E, S, T, A, B](parser: Parser[E, S, T, A], separator: Parser[E, S, T, B]): Parser[E, S, T, (List[A], List[B])] = {
    app2(_buildSepByValue[A, B],
        parser,
        many0(app2(_pair[B, A], separator, parser)))
  }

  def sepBy0[E, S, T, A, B](parser: Parser[E, S, T, A], separator: Parser[E, S, T, B]): Parser[E, S, T, (List[A], List[B])] = {
    optionalV(sepBy1(parser, separator), (List(), List()))
  }

  class Itemizer[E, S, T](val f: (T, S) => S) {
    val item: Parser[E, S, T, T] = new Parser((xs, s) => xs match {
      case Nil => Zero
      case (y :: ys) => Helpers.good(ys, f(y, s), y)
    })

    def literal(x: T): Parser[E, S, T, T] = check(y => x == y, this.item)
    def satisfy(predicate: T => Boolean): Parser[E, S, T, T] = check(predicate, this.item)
    def not1[A](parser: Parser[E, S, T, A]): Parser[E, S, T, T] = seq2R(not0(parser), this.item)
    def string(elements: List[T]): Parser[E, S, T, List[T]] = seq(elements.map(this.literal))
    def oneOf(elements: Set[T]): Parser[E, S, T, T] = satisfy(x => elements.contains(x))
  }

  def _bump(char: Char, position: (Int, Int)): (Int, Int) = {
    char match {
      case '\n' => (position._1 + 1, 1)
      case _ => (position._1, position._2 + 1)
    }
  }

  def basic[E, S, T](): Itemizer[E, S, T] = new Itemizer((_, s) => s)
  def position[E](): Itemizer[E, (Int, Int), Char] = new Itemizer(_bump)
  def count[E, T](): Itemizer[E, Int, T] = new Itemizer((_, s) => s + 1)

  // TODO some implicits?

  //implicit def partial2[A, B, C](f: (A, B) => C) = (a: A) => (b: B) => f(a, b)
  //implicit def partial3[A, B, C, D](f: (A, B, C) => D) = (a: A) => (b: B) => (c: C) => f(a, b, c)
  //implicit def partial4[A, B, C, D, E](f: (A, B, C, D) => E) = (a: A) => (b: B) => (c: C) => (d: D) => f(a, b, c, d)

  def cut[S, T, A](message: String, parser: Parser[List[(String, S)], S, T, A]): Parser[List[(String, S)], S, T, A] = {
    bind(getState(), (state: S) => commit(List((message, state)), parser))
  }

  def addError[E, S, T, A](e: E, parser: Parser[List[(E, S)], S, T, A]): Parser[List[(E, S)], S, T, A] = {
    bind(getState(), (state: S) => mapError((es: List[(E, S)]) => (e, state) :: es, parser))
  }

  case class Node[S, +A](val name: String, val position: (S, S), val tree: A)
  def node[S, T, A](name: String, parser: Parser[List[(String, S)], S, T, A]): Parser[List[(String, S)], S, T, Node[S, A]] = {
    val f = (start: S) => (tree: A) => (end: S) => new Node(name, (start, end), tree)
    addError(name, app3(f, getState(), parser, getState()))
  }
}
