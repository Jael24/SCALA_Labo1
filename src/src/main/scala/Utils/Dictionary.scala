package Utils

/**
  * Contains the dictionary of the application, which is used to validate, correct and normalize words entered by the
  * user.
  */
object Dictionary {
  // This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
  // we want to normalize the words "veux" and "aimerais" in on unique term: "vouloir").
  val dictionary: Map[String, String] = Map(
    "bonjour" -> "bonjour",
    "hello" -> "bonjour",
    "yo" -> "bonjour",
    "je" -> "je",
    "j" -> "je",
    "suis" -> "etre",
    "veux" -> "vouloir",
    "aimerais" -> "vouloir",
    "bière" -> "biere",
    "bières" -> "biere",
    "croissant" -> "croissant",
    "croissants" -> "croissant",
    "et" -> "et",
    "ou" -> "ou",
    "svp" -> "svp",
    "stp" -> "svp"
  )
}
