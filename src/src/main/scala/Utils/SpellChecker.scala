package Utils

import scala.math.min
import Dictionary.dictionary

object SpellChecker {
  /**
   * Calculate the Levenshtein distance between two words.
   *
   * @param s1 the first word
   * @param s2 the second word
   * @return an integer value, which indicates the Levenshtein distance between "s1" and "s2"
   */
  def stringDistance(s1: String, s2: String): Int = {
    1
  }

  /**
   * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
   * function. If the word is a number, this function just returns it.
   *
   * @param misspelledWord the misspelled word to correct
   * @return the closest word from "misspelledWord"
   */
  // TODO - Step 2
  def getClosestWordInDictionary(misspelledWord: String): String = ???
}
