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
    listDistance(s1.toList, s2.toList)
  }

  /**
   * Helper that actually calculates the Levenshtein distance between two words.
   *
   * @param l1 the first word
   * @param l2 the second word
   * @return an integer value, which indicates the Levenshtein distance between "s1" and "s2"
   */
  def listDistance(l1: List[Char], l2: List[Char]): Int = (l1, l2) match {
    case (Nil, l2) => l2.length
    case (l1, Nil) => l1.length
    case (x :: xs, y :: ys) => if (x == y) {
      listDistance(xs, ys)
    } else {
      1 + min(listDistance(l1, ys), min(listDistance(xs, l2), listDistance(xs, ys)))
    }
  }

  /**
   * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
   * function. If the word is a number, this function just returns it.
   *
   * @param misspelledWord the misspelled word to correct
   * @return the closest word from "misspelledWord"
   */
  def getClosestWordInDictionary(misspelledWord: String): String = {
    var score = ("", Int.MaxValue)

    // Return the word if it's a number or a username
    if (misspelledWord.forall(_.isDigit) || misspelledWord.startsWith("_")) {
      return misspelledWord
    } else {
      for (key <- dictionary.keys) {
        val temp = stringDistance(misspelledWord, key)

        if (temp < score._2) {
          score = (key, temp)
        }
      }
    }

    dictionary(score._1)
  }
}
