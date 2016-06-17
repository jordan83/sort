import com.peoplepattern.text.Implicits._

case class Listing(title: String, manufacturer: String, currency: String, price: String) {
  lazy val tokens = (title + manufacturer).tokens.toList
}
case class Product(productName: String, manufacturer: String, family: Option[String], model: String, announcedDate: String)  {
  lazy val tokens = (manufacturer + " " + family.getOrElse("") + " " + model).tokens.toList
}

class ListingCollection(listings: Array[Listing]) {
  type InvertedIndex = Map[String, Set[Int]]
  def InvertedIndex() = Map[String, Set[Int]]().withDefaultValue(Set())

  def search(p: Product) = {
    // Find fuzzy product matches for all tokens in the given product. Duplicates can exist if a listing
    // matches more than one token.
    val matchingListings = tokenTolerancePairs(p).flatMap{ case (token, tolerance) => listingsWithToken(token, tolerance) }

    // Find the frequency of a listing. We're looking for listings that match every token, so filter out any listings
    // that don't occur once for every token in the given product
    val documentFrequencies = matchingListings.foldLeft(Map[Listing, Int]().withDefaultValue(0))((acc, l) =>acc + (l -> (acc(l) + 1)))
    documentFrequencies.filter{ case (listing, numMatches) => numMatches >= p.tokens.size }.map(_._1)
  }

  // TODO: Makes sense to be slightly more permissive on family typos as well as manufacturer. Continue to be
  // strict about the model numbers, single character changes can make a large difference. Makes sense
  // to lower case all the text though first though.
  private def tokenTolerancePairs(p: Product) =
    p.manufacturer.tokens.toList.map((_, 1)) ++ (p.family.getOrElse("") + " " + p.model).tokens.toList.map((_, 0))

  private def listingsWithToken(token: String, tolerance: Int) =
    tree.find(token, tolerance).flatMap(t => invertedIndex(t).map(listings(_))).toSet

  private lazy val tree = {
    val keys = invertedIndex.keys
    keys.tail.foldLeft(BKTree(keys.head))((acc, k) => acc.add(k))
  }

  private lazy val invertedIndex = {
    def addDocToIndex(invIndex: InvertedIndex, l: Listing, index: Int) = {
      l.tokens.foldLeft(invIndex){(acc, t) => acc + (t -> (acc(t) + index))}
    }

    listings.zipWithIndex.foldLeft(InvertedIndex()){(acc, e) =>
      addDocToIndex(acc, e._1, e._2)
    }
  }
}

object ListingCollection {
  def apply(listings: Array[Listing]) = new ListingCollection(listings)
}

object Main {

  def main(args: Array[String]) = {
    val listings = FileUtils.loadListings
    val products = FileUtils.loadProducts

    val c = ListingCollection(listings)
    // TODO: Consider filtering out listings whose prices are significantly far away from the mean price.
    val allProductMatches = products.par.map(p => (p.productName, c.search(p))).filter(_._2.size > 0)
    FileUtils.writeResults("results.txt", allProductMatches)
  }
}