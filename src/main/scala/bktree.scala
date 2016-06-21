case class BKTree(val str: String, val children: Map[Int, BKTree] = Map[Int, BKTree]()) {

  def add(word: String): BKTree = {
    val dist = StringUtils.levenshtein(str, word)
    val newChildren = if (children.contains(dist)) {
      children + (dist -> children(dist).add(word))
    }
    else {
      children + (dist ->  new BKTree(word))
    }
    BKTree(str, newChildren)
  }

  def find(word: String, tolerance: Int): List[String] = {
    val dist = StringUtils.levenshtein(word, str)
    lazy val childResults = ((dist - tolerance) to (dist + tolerance)).
      toList.
      flatMap(children.get(_)). // flat map to pull out only successful queries against the child list
      flatMap(_.find(word, tolerance))
    if (dist <= tolerance) str :: childResults else childResults
  }

}