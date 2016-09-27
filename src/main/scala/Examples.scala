import com.unparse_scala.{JSONParser, Parser, Zero, Error, Success}

/**
  * Created by matt.fenwick on 9/21/16.
  */

object Examples extends App {
  SomeParseExamples.jsonParsing()
  TyingTheKnot.eg1()
  //println(TyingTheKnot.v1.parse(List(1,2,3,4,5,8,8), 0))
}

object TyingTheKnot {
  def eg1(): Unit = {
    import Parser._
    //def cons[T](x: T, xs: List[T]): List[T] = x :: xs
    val cons = (x: Int, xs: List[Int]) => x :: xs
    val thing = count[Int]().oneOf(List(1,2,3,4).toSet)
    lazy val v1: Parser[String, Int, Int, List[Int]] = app2(cons.curried, thing, optionalV(v2, Nil))
    lazy val v2: Parser[String, Int, Int, List[Int]] = app2(cons.curried, thing, optionalV(v1, Nil))

    println(v1.parse(List(1,2,3,4,5,8,8), 0))
  }
}

object SomeParseExamples {

  def jsonParsing(): Unit = {
    val p = JSONParser.jsonValue
    val parsed = p.parse("[1,false,null,\"abc\",{\"123\":123}]".toList, (1, 1))
    println(s"parsed: ${parsed}")
    parsed match {
      case Zero => println("oops")
      case Error(e) => println(s"error: $e")
      case Success(s) => println(s"success: ${s._3.toString}")
    }
  }

  def debug[E, S, T, A](parser: Parser[E, S, T, A], message: String): Parser[E, S, T, A] = {
    Parser.seq2R(Parser.bind(Parser.getState(), (s: S) => {
      println(s"state for $message: $s")
      Parser.pure(Unit)
    }), parser)
  }

}