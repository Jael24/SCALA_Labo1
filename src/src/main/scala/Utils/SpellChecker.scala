package Utils

import scala.math.max
import Dictionary.dictionary

object SpellChecker {
  /**
   * Calculate the Levenshtein distance between two words.
   *
   * @param s1 the first word
   * @param s2 the second word
   * @return an integer value, which indicates the Levenshtein distance between "s1" and "s2"
   */
  // TODO comment (and translate to tail rec)
  // TODO store the distance in an array?
  def stringDistance(s1: String, s2: String): Int = (s1.toList, s2.toList) match {
    case (_, _) if (s1.isEmpty || s2.isEmpty) => max(s1.length, s2.length)
    case (x :: xs, y :: ys) => if (x == y) {
      stringDistance(xs.toString, ys.toString)
    } else {
      2 // TODO finish
    }
  }

  /**
   * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
   * function. If the word is a number, this function just returns it.
   *
   * @param misspelledWord the misspelled word to correct
   * @return the closest word from "misspelledWord"
   */
  // TODO - Step 2
  def getClosestWordInDictionary(misspelledWord: String): String = {
    var score = ("", Int.MaxValue)

    // Check if not a number or a pseudo
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

    score._1
  }
}
