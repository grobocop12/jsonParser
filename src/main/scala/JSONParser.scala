import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class JSONParser {
  def parse(tokens: List[(String, String)]): JSON = {
    val iterator = tokens.iterator
    iterator.next() match {
      case ("{", "") => parseObject(iterator)
      case ("[", "") => parseList(iterator)
      case (a, b) => throw new RuntimeException("Invalid token " + a)
    }
  }

  def parseObject(iterator: Iterator[(String, String)]): JSONObject = {
    val fields = new mutable.HashMap[String, Any]()
    var token = iterator.next()
    while (token._1 != "}") {
      val key = (token, iterator.next(), iterator.next())
      key match {
        case (("string", a), (":", _), ("string", b)) => fields.addOne(a, b)
        case (("string", a), (":", _), ("number", b)) => fields.addOne(a, b.toDouble)
        case (("string", a), (":", _), ("literal", b)) => fields.addOne(a, parseLiteral(b))
        case (("string", a), (":", _), ("{", _)) => fields.addOne(a, parseObject(iterator))
        case (("string", a), (":", _), ("[", _)) => fields.addOne(a, parseList(iterator))
      }
      val comma = iterator.next()
      comma match {
        case (",", _) => token = iterator.next()
        case ("}", "") => token = comma
      }
    }
    new JSONObject(fields.toMap)
  }

  def parseList(iterator: Iterator[(String, String)]): JSONList = {
    val elements = new ListBuffer[Any]()
    var token = iterator.next()
    while (token._1 != "]") {
      token match {
        case ("string", a) => elements.addOne(a)
        case ("number", a) => elements.addOne(a.toDouble)
        case ("literal", a) => elements.addOne(parseLiteral(a))
        case ("{", _) => elements.addOne(parseObject(iterator))
        case ("[", _) => elements.addOne(parseList(iterator))
      }
      val comma = iterator.next()
      comma match {
        case (",", _) => token = iterator.next()
        case ("]", "") => token = comma
      }
    }
    new JSONList(elements.toList)
  }

  def parseLiteral(literal: String): Any = literal match {
    case "true" => true
    case "false" => false
    case "null" => null
  }
}

abstract class JSON

class JSONObject(val fields: Map[String, Any]) extends JSON

class JSONList(val list: List[Any]) extends JSON