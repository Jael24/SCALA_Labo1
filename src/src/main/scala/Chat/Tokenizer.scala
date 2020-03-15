package Chat

import Chat.Tokens._
import Utils.Dictionary.dictionary
import Utils.SpellChecker.getClosestWordInDictionary

import scala.collection.mutable.ListBuffer

class Tokenizer(input: String) {

  var tokens = new ListBuffer[(String, Token)]()
  var count: Int = -1

  /**
   * Separate the user's input into tokens.
   */
  def tokenize(): Unit = {
    val toRemove = ".,!?*".toSet
    val inputNormalized = input.filterNot(toRemove)
      .replace('\'', ' ')
      .replaceAllLiterally("  ", " ")
      .split(" ")
      .map(w => getClosestWordInDictionary(w) -> matchToken(w)).toVector
    for (i <- 0 until inputNormalized.length) {
      tokens.addOne(inputNormalized.apply(i))
    }
    tokens.addOne("EOL" -> EOL)
  }

  /**
   * Get the next token of the user input, or OEL if there is no more token.
   *
   * @return a tuple that contains the string value of the current token, and the identifier of the token
   */
  def nextToken(): (String, Token) = {
    count += 1
    tokens.toVector.apply(count)
  }

  def matchToken(s: String): Token = {
    val word = if (dictionary.contains(s)) s else getClosestWordInDictionary(s)
    if (!dictionary.contains(word)) {
      if (word.startsWith("_")) PSEUDO else if (word forall Character.isDigit) NUM else UNKNOWN
    } else {
      dictionary.apply(word) match {
        case "bonjour" => BONJOUR
        case "je" => JE
        case "etre" => ETRE
        case "vouloir" => VOULOIR
        case "et" => ET
        case "ou" => OU
        case "biere" => BIERE
        case "croissant" => CROISSANT
        case _ => UNKNOWN
      }
    }
  }
}
