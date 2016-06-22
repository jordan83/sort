import com.peoplepattern.text.Implicits._

case class Listing(title: String, manufacturer: String, currency: String, price: String) {
  lazy val tokens = (title.tokens ++ manufacturer.tokens).toList

  lazy val parsedPrice = {
    try {
      Some(price.toDouble)
    } catch {
      case e: Exception => None
    }
  }
}
case class Product(productName: String, manufacturer: String, family: Option[String], model: String, announcedDate: String)

class ListingCollection(listings: Array[Listing]) {
  type InvertedIndex = Map[String, Set[Int]]
  def InvertedIndex() = Map[String, Set[Int]]().withDefaultValue(Set())

  def search(p: Product) = {

    def tokenTolerancePairs(p: Product) =
      (p.manufacturer + " " + p.family.getOrElse("")).toLowerCase.tokens.toList.map((_, 2)) ++ p.model.toLowerCase.tokens.toList.map((_, 0))

    def listingsWithToken(token: String, tolerance: Int) = tree.find(token, tolerance).flatMap(t => invertedIndex(t).map(listings(_))).toSet

    def filterByPrice(listings: Iterable[Listing]) = {
      val listingsWithPrices = listings.flatMap(l => l.parsedPrice.map(p => l))

      // Compute the standard deviation of the sample that includes matching manufacturers
      val manufacturerMatches = listingsWithPrices.filter(l => BKTree.levenshtein(l.manufacturer.toLowerCase, p.manufacturer.toLowerCase) <= 2)
      val size = manufacturerMatches.size.toDouble
      val mean = manufacturerMatches.map(_.parsedPrice.get).sum / size
      val variance = manufacturerMatches.map(l => Math.pow(l.parsedPrice.get - mean, 2)).sum / size
      val standardDeviation = Math.sqrt(variance)

      // Filter listings with prices that are more than 2 standard deviations from the sample mean. This
      // Should be enough to pick up the real outliers and allows us to consider listings where the manufacturer
      // doesn't match.
      listingsWithPrices.filter(l => Math.abs(l.parsedPrice.get - mean) <= (standardDeviation * 2))
    }

    // Find fuzzy product matches for all tokens in the given product. Duplicates can exist if a listing
    // matches more than one token.
    val matchingListings = tokenTolerancePairs(p).flatMap{ case (token, tolerance) => listingsWithToken(token, tolerance) }

    // Find the frequency of a listing. We're looking for listings that match every token, so filter out any listings
    // that don't occur once for every token in the given product
    val listingFrequencies = matchingListings.foldLeft(Map[Listing, Int]().withDefaultValue(0))((acc, l) =>acc + (l -> (acc(l) + 1)))

    // Filter out anything that's got a price that's way out of whack.
    filterByPrice(
      listingFrequencies.
      filter{ case (listing, numMatches) => numMatches >= tokenTolerancePairs(p).size }.
      map(_._1)
    )
  }

  private lazy val tree = {
    val keys = invertedIndex.keys
    keys.tail.foldLeft(BKTree(keys.head))((acc, k) => acc.add(k))
  }

  private lazy val invertedIndex = {
    def addListingToIndex(invIndex: InvertedIndex, l: Listing, index: Int) = {
      l.tokens.map(_.toLowerCase).foldLeft(invIndex){(acc, t) => acc + (t -> (acc(t) + index))}
    }

    listings.zipWithIndex.foldLeft(InvertedIndex()){(acc, e) =>
      addListingToIndex(acc, e._1, e._2)
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

    val allProductMatches = products.
      par.
      map(p => (p.productName, c.search(p))).
      filter(_._2.size > 0)
    FileUtils.writeResults("results.txt", allProductMatches)
  }
}
