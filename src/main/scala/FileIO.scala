import scala.io.Source
import scala.util.Using
import org.json4s.Formats
import org.json4s.jackson.JsonMethods.parse

object FileIO {

  type Subscription = (String, String) // (subredditName, url)
  private case class RawSubscription(name: String, url: String)

  type Post = (String, String, String, String) // (subreddit, title, selftext,)
  private case class RawPost(title: String, selftext: String, created_utc: String, date: String)

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
def downloadFeed(url: String, formats: Formats): List[Post] = {
  val jsonPosts = Using.resource(Source.fromURL(url)(scala.io.Codec.UTF8)) { src =>
    src.getLines().mkString("\n")
  }

  println(jsonPosts) // Esto está bien para debuggear

  // ESTO es lo que debe ir al final para que sea el valor de retorno:
  parse(jsonPosts)
    .extract[List[RawPost]](formats, manifest[List[RawPost]])
    .map(p => (url, p.title, p.selftext, p.created_utc)) 
    // Nota: agregué 'url' como primer elemento para que coincida con Post = (String, String, String, String)
}
 
}
