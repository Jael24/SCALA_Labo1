import Chat.Tokenizer
import Chat.Tokens._
import Utils.ClinksCalculator.calculateCombination

import scala.io.StdIn

object Main extends App {
  println("Bienvenue au Chill-Out !")

  while (true) {
    // Convert the user input to lower case, then take an action depending on the value.
    StdIn.readLine.toLowerCase match {
      case "quitter" => println("Adieu."); System.exit(0)
      case "santé !" => {
        for (i <- 2 to 6) {
          println(s"Nombre de *clinks* pour un santé de $i personnes : ${calculateCombination(i, 2)}.")
        }
      }
      case s => {
        val tokenizer = new Tokenizer(s)

        // Tokenize the user input.
        tokenizer.tokenize()

        var currentToken: (String, Token) = null

        // Display every token.
        do {
          currentToken = tokenizer.nextToken()
          println(currentToken)
        } while (currentToken._2 != EOL)

        println("==================================================")
      }
    }
  }
}
