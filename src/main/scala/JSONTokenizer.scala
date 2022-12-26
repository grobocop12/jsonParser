import scala.collection.mutable.ListBuffer
import scala.io.Source

class JSONTokenizer(val filename: String) {
  def tokenize(): List[(String, String)] = {
    val source = Source.fromFile(filename)
    val iterator = source.iterator
    try {
      readTokens(iterator)
    } catch {
      case e: Exception =>
        e.printStackTrace()
        throw new RuntimeException(e)
    } finally {
      source.close()
    }
  }


  def readTokens(iterator: Iterator[Char]): List[(String, String)] = {
    val tokens = new ListBuffer[(String, String)]()
    while (iterator.hasNext) {
      iterator.next() match {
        case ' ' | '\n' | '\r' | '\t' => ()
        case '{' => tokens.addOne(("{", ""))
        case '}' => tokens.addOne(("}", ""))
        case '[' => tokens.addOne(("[", ""))
        case ']' => tokens.addOne(("]", ""))
        case ':' => tokens.addOne((":", ""))
        case ',' => tokens.addOne((",", ""))
        case '\"' => readString(tokens, iterator)
        case x if x == '-' || (x >= '0' && x <= '9') => readNumber(x, tokens, iterator)
        case x if x == 't' || x == 'f' || x == 'n' => readLiteral(x, tokens, iterator)
        case _ =>
      }
    }
    tokens.toList
  }

  def readString(tokens: ListBuffer[(String, String)], iterator: Iterator[Char]): Unit = {
    var string = ""
    var lastChar: Character = null
    var char = iterator.next()
    while (char != '"' || lastChar == '\\') {
      string += char
      lastChar = char
      char = iterator.next()
    }
    tokens.addOne(("string", string))
  }

  def readNumber(firstChar: Char, tokens: ListBuffer[(String, String)], iterator: Iterator[Char]): Unit = {
    var numberString = firstChar.toString
    var char = iterator.next()
    while ((char >= '0' && char <= '9') || char == 'e' || char == 'E' || char == '+' || char == '-' || char == '.') {
      numberString += char
      char = iterator.next()
    }
    tokens.addOne(("number", numberString))
    char match {
      case ' ' | '\t' | '\n' | '\r' => ()
      case '{' => tokens.addOne(("{", ""))
      case '}' => tokens.addOne(("}", ""))
      case '[' => tokens.addOne(("[", ""))
      case ']' => tokens.addOne(("]", ""))
      case ':' => tokens.addOne((":", ""))
      case ',' => tokens.addOne((",", ""))
      case _ => throw new RuntimeException("Unexpected token " + char)
    }
  }

  def readLiteral(firstChar: Char, tokens: ListBuffer[(String, String)], iterator: Iterator[Char]): Unit = {
    var literalString = firstChar.toString
    var char = iterator.next()
    while ((char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')) {
      literalString += char
      char = iterator.next()
    }
    if (literalString != "null" && literalString != "true" && literalString != "false")
      throw new RuntimeException("Unknown literal: " + literalString)
    tokens.addOne(("literal", literalString))
    char match {
      case ' ' | '\t' | '\n' | '\r' => ()
      case '{' => tokens.addOne(("{", ""))
      case '}' => tokens.addOne(("}", ""))
      case '[' => tokens.addOne(("[", ""))
      case ']' => tokens.addOne(("]", ""))
      case ':' => tokens.addOne((":", ""))
      case ',' => tokens.addOne((",", ""))
      case _ => throw new RuntimeException("Unexpected token " + char)
    }
  }
}

