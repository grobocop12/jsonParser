

object Main {
  def main(args: Array[String]): Unit = {
    val tokenizer = new JSONTokenizer(args(0))
    val tokens = tokenizer.tokenize()
    val parser = new JSONParser()
    val json = parser.parse(tokens)
    println()
  }
}