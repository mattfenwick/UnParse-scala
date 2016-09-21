/**
  * Created by matt.fenwick on 9/21/16.
  */

object Examples extends App {
  SomeParseExamples.jsonParsing()
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