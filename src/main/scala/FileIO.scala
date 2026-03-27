import scala.io.Source
import scala.util.Using
import org.json4s.Formats
import org.json4s.jackson.JsonMethods.parse

object FileIO {

  type Subscription = (String, String) // (subredditName, url)
  private case class RawSubscription(name: String, url: String)

  // Pure function to read subscriptions from a JSON file
  def readSubscriptions(path: String, formats: Formats): List[Subscription] = {

  val jsonSubscriptions = Using.resource(Source.fromFile(path, "UTF-8")) { src =>
    src.getLines().mkString("\n")
  }

  parse(jsonSubscriptions)
    .extract[List[RawSubscription]](formats, manifest[List[RawSubscription]])
    .map(s => (s.name, s.url))
}

  // Pure function to download JSON feed from a URL
  def downloadFeed(url: String): String =
    Using.resource(Source.fromURL(url)(scala.io.Codec.UTF8)) { source =>
      source.mkString
    }
}
