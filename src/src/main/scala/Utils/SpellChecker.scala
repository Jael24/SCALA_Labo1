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
    // TODO comment (and translate to tail rec)
    val l1 = 0 to s1.length toArray
    val l2 = 0 to s2.length toArray

    val d = Array(l1, l2)

    var cost = 0

    for (i <- 1 to s1.length) {
      for (j <- 1 to s2.length) {
        if (s1.charAt(i - 1) == s2.charAt(j - 1))
          cost = 0
        else
          cost = 1

        d(i)(j) = min(
          min(
            d(i - 1)(j) + 1, // Remove a character
            d(i)(j - 1) + 1), // Add a new character
          d(i - 1)(j - 1) + cost // Substitute a character for another
        )

        if (i > 1 && j > 1 && s1.charAt(i) == s2.charAt(j - 1) && s1.charAt(i - 1) == s2.charAt(j)) {
          d(i)(j) = min(
            d(i)(j),
            d(i - 2)(j - 2) + cost
          )
        }
      }
    }

    d(s1.length)(s2.length)
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
