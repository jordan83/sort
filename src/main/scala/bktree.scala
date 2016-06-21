case class BKTree(val str: String, val children: Map[Int, BKTree] = Map[Int, BKTree]()) {

  def add(word: String): BKTree = {
    val dist = BKTree.levenshtein(str, word)
    val newChildren = if (children.contains(dist)) {
      children + (dist -> children(dist).add(word))
    }
    else {
      children + (dist ->  new BKTree(word))
    }
    BKTree(str, newChildren)
  }

  def find(word: String, tolerance: Int): List[String] = {
    val dist = BKTree.levenshtein(word, str)
    lazy val childResults = ((dist - tolerance) to (dist + tolerance)).
      toList.
      flatMap(children.get(_)). // flat map to pull out only successful queries against the child list
      flatMap(_.find(word, tolerance))
    if (dist <= tolerance) str :: childResults else childResults
  }

}

object BKTree {
  // Inspired by wikipedia implementation.
  def levenshtein(str1: String, str2: String): Int = {
    val lenStr1 = str1.length
    val lenStr2 = str2.length

    val d = Array.ofDim[Int](lenStr1 + 1, lenStr2 + 1)

    for (i <- 0 to lenStr1) d(i)(0) = i
    for (j <- 0 to lenStr2) d(0)(j) = j

    for (i <- 1 to lenStr1; j <- 1 to lenStr2) {
      val cost = if (str1(i - 1) == str2(j-1)) 0 else 1

      d(i)(j) = Array(
        d(i-1)(j) + 1,
        d(i )(j-1) + 1,
        d(i-1)(j-1) + cost
      ).min
    }

    d(lenStr1)(lenStr2)
  }
}