package Chat

import Chat.Tokens._
import Utils.Dictionary.dictionary

class Tokenizer(input: String) {
  /**
    * Separate the user's input into tokens.
    */
  def tokenize(): Unit = {
    val toRemove = ".,!?*".toSet
    val inputNormalized = input.filterNot(toRemove)
      .replace('\'', ' ')
      .replaceAllLiterally("  ", " ")
      .split(" ")
      .map(w => w -> matchToken(dictionary.apply(w))).toMap
    println(inputNormalized)
  }

  /**
    * Get the next token of the user input, or OEL if there is no more token.
  	* @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  // TODO - Step 3
  def nextToken(): (String, Token) = ???

  def matchToken(s: String): Token = s match {
    case "bonjour" => BONJOUR
    case "je" => JE
    case "etre" => ETRE
    case "vouloir" => VOULOIR
    case "et" => ET
    case "ou" => OU
    case "biere" => BIERE
    case "croissant" => CROISSANT
    case "\n" => EOL
    case _ => UNKNOWN
  }
}
