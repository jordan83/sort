import java.io.{File, PrintWriter}

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

import scala.collection.parallel.mutable.ParArray
import scala.collection.immutable.Iterable
import scala.io.Source

object FileUtils {
  implicit val formats = DefaultFormats

  def loadListings = fileToJsonList("listings.txt").map(_.extract[Listing])

  def loadProducts = {
    def transformProductJson(json: JValue) = json.transformField {
      case ("product_name", x) => ("productName", x)
      case ("announced-date", x) => ("announcedDate", x)
    }
    fileToJsonList("products.txt").map(transformProductJson(_)).map(_.extract[Product])
  }

  def writeResults(file: String, results: ParArray[(String, Iterable[Listing])]) = {
    val pw = new PrintWriter(new File(file))
    results.foreach{ kv =>
      val json = (
        ("product_name" -> kv._1) ~
          ("listings" -> kv._2.map{l =>
            (
              ("title" -> l.title) ~
                ("manufacturer" -> l.manufacturer) ~
                ("currency" -> l.currency) ~
                ("price" -> l.price)
              )
          })
        )

      pw.write(compact(render(json)))
      pw.write("\n")
    }
  }

  private def fileToJsonList(fileName: String) = {
    val source = Source.fromFile(fileName)
    try {
      val fileData = source.mkString
      val entries = fileData.split("\n")
      entries.map(parse(_))
    } finally {
      source.close
    }
  }
}