/**
  * Created by matt.fenwick on 9/17/16.
  */
object JSONParser {
  import Parser._

  type S = (Int, Int)
  type E = List[(String, S)] // TODO or maybe List[String] ?
  type T = Char

  val itemizer = position[E]
  import itemizer._

  def quantity[E, S, T, A](parser: Parser[E, S, T, A], num: Int): Parser[E, S, T, List[A]] = {
    seq((1 to num).toList.map( (_) => parser) )
  }

  // TODO why can't I use this implicitly?
  // implicit def stringToList(string: String): List[Char] = string.toList

  type JSONNode[+A] = Node[(Int, Int), A]
  // TODO added `case` to enable `Decimal.apply _ curried` syntax.  don't understand why that works, though
  case class Decimal(val dot: Char, val digits: List[Char])
  case class Exponent(val e: Char, val sign: Option[Char], val power: List[Char])

  val whitespace = many0(oneOf(" \t\n\r".toSet))
  val _digit = oneOf("0123456789".toSet)
  val _digits = many1(_digit)
  val _decimal = node[S, T, Decimal]("decimal", app2(Decimal.apply _ curried, literal('.'), cut("digits", _digits)))
  val _exponent = node[S, T, Exponent]("exponent", app3(Exponent.apply _ curried,
            oneOf("eE".toSet),
            optional(oneOf("+-".toSet)),
            cut("power", _digits)))
  val _integer = addError[String, S, T, List[Char]]("invalid leading 0", bind(_digits, (ds: List[Char]) => ds match {
    case ('0' :: _ :: _) => error(List())
    case _ => pure(ds)
  }))
  val _number_1 = node[S, T, JSONNumber]("number", app4(JSONNumber.apply _ curried,
    fmap[E, S, T, Char, Option[Char]]((c: Char) => Some(c), literal('-')),
    cut("digits", _integer),
    optional(_decimal),
    optional(_exponent)))
  val _number_2 = node[S, T, JSONNumber]("number", app4(JSONNumber.apply _ curried,
    pure[E, S, T, Option[Char]](None),
    _integer,
    optional(_decimal),
    optional(_exponent)))
  val _number = alt(List(_number_1, _number_2))

  sealed trait CharNode
  case class SimpleChar(val char: Char) extends CharNode
  case class EscapeChar(val open: Char, val value: Char) extends CharNode
  case class UnicodeEscape(val open: List[Char], val value: List[Char]) extends CharNode

  val _control = addError[String, S, T, Char]("invalid control character",
    seq2L(satisfy((c) => c < 32), error(List())))
  val _char = node[S, T, SimpleChar]("character", app(SimpleChar.apply _, not1(alt(List(oneOf("\\\"".toSet), _control)))))
  val _escape = node[S, T, EscapeChar]("escape", app2(EscapeChar.apply _ curried, literal('\\'), cut("simple escape", oneOf("\\/bfnrt".toSet))))
  val _hexC = oneOf("0123456789abcdefABCDEF".toSet)
  val _unic = node[S, T, UnicodeEscape]("unicode escape", app2(UnicodeEscape.apply _ curried, string("\\u".toList), cut("4 hexidecimal digits", quantity(_hexC, 4))))
  // TODO wow, this is so messed up.  definitely a better way to do this
  def upcast[M[+_], V, U <: V](u: M[U]): M[V] = u
  val _jsonString = node[S, T, JSONString]("string", app3(JSONString.apply _ curried,
    literal('"'),
    many0(alt[E, S, T, JSONNode[CharNode]](List(fmap(upcast[JSONNode, CharNode, SimpleChar], _char),
      fmap(upcast[JSONNode, CharNode, EscapeChar], _escape),
      fmap(upcast[JSONNode, CharNode, UnicodeEscape], _unic)))),
    literal('"')))
  val _keywords = List("true", "false", "null")
  val _keyword = node[S, T, JSONKeyword]("keyword", app(JSONKeyword.apply _, alt[E, S, T, List[Char]](_keywords.map((x: String) => string(x.toList)))))

  def token[E, S, T, A](parser: Parser[E, S, T, A]): Parser[E, S, T, A] =  seq2L(parser, whitespace)

  val jsonString: Parser[E, S, T, JSONNode[JSONString]] = token(_jsonString)
  val number     = token(_number)
  val keyword    = token(_keyword)
  val os         = token(literal('['))
  val cs         = token(literal(']'))
  val oc         = token(literal('{'))
  val cc         = token(literal('}'))
  val comma      = token(literal(','))
  val colon      = token(literal(':'))

  sealed trait JSONValue
  case class JSONArray(val open: Char, val values: (List[JSONNode[JSONValue]], List[Char]),
                       val close: Char) extends JSONValue
  case class JSONObject(val open: Char, val pairs: (List[JSONNode[JSONKVPair]], List[Char]),
                        val close: Char) extends JSONValue
  case class JSONNumber(val sign: Option[Char], val integer: List[Char], val decimal: Option[JSONNode[Decimal]],
                        val exponent: Option[JSONNode[Exponent]]) extends JSONValue
  case class JSONString(val open: Char, val value: List[JSONNode[CharNode]], val close: Char) extends JSONValue
  case class JSONKeyword(val text: List[Char]) extends JSONValue

  case class JSONKVPair(val key: JSONNode[JSONString], val colon: Char, val value: JSONNode[JSONValue])

  var jsonValue: Parser[E, S, T, JSONNode[JSONValue]] = Parser.error(List(("unimplemented", (0, 0))))
  val jsonKVPair = node[S, T, JSONKVPair]("key/value pair", app3(JSONKVPair.apply _ curried,
    jsonString,
    cut("colon", colon),
    cut("value", jsonValue)))
  val jsonArray = node[S, T, JSONArray]("array", app3(JSONArray.apply _ curried,
    os,
    sepBy0(jsonValue, comma),
    cs))
  val jsonObject = node[S, T, JSONObject]("object", app3(JSONObject.apply _ curried,
    oc,
    sepBy0(jsonKVPair, comma),
    cc))
  jsonValue.parse = alt[E, S, T, JSONNode[JSONValue]](List(
    fmap(upcast[JSONNode, JSONValue, JSONKeyword], keyword),
    fmap(upcast[JSONNode, JSONValue, JSONNumber], number),
    fmap(upcast[JSONNode, JSONValue, JSONString], jsonString),
    fmap(upcast[JSONNode, JSONValue, JSONArray], jsonArray),
    fmap(upcast[JSONNode, JSONValue, JSONObject], jsonObject)
  )).parse
  // TODO 1: use contravariant parameters to avoid obnoxious type declarations
  // TODO 2: handle string escapes
  // TODO 3: good error messaging
  val _json = jsonValue
  val json = app3[E, S, T, List[Char], JSONNode[JSONValue], Unit, JSONNode[JSONValue]](
    (_: List[Char]) => (v: JSONNode[JSONValue]) => (_: Unit) => v,
    whitespace,
    cut("json value", _json),
    cut("unparsed input remaining", not0(item)))
}
