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
    // Define a function for the lists
    def listDistance(l1: List[Char], l2: List[Char]): Int = (l1, l2) match {
      // If one of the lists is empty, return the length of the other
      case (Nil, l2) => l2.length
      case (l1, Nil) => l1.length
      case (x :: xs, y :: ys) => if (x == y) {
        // If the elements are identical, don't add to the distance
        listDistance(xs, ys)
      } else {
        // If they aren't, add 1 to the distance and take the shortest path to the word
        1 + min(listDistance(l1, ys), min(listDistance(xs, l2), listDistance(xs, ys)))
      }
    }

    listDistance(s1.toList, s2.toList)
  }

  /**
   * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
   * function. If the word is a number, this function just returns it.
   *
   * @param misspelledWord the misspelled word to correct
   * @return the closest word from "misspelledWord"
   */
  def getClosestWordInDictionary(misspelledWord: String): String =
    if (misspelledWord.forall(_.isDigit) || misspelledWord.startsWith("_")) {
      // Return the word if it's a number or a username
      misspelledWord
    } else {
      // Else, get the smallest distance between the given word and a word in the dictionary
      var score = ("", Int.MaxValue)

      for (key <- dictionary.keys) {
        val temp = stringDistance(misspelledWord, key)

        if (temp < score._2) {
          score = (key, temp)
        }
      }

      dictionary(score._1)
    }
}
