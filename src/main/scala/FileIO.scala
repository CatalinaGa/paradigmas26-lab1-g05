import scala.io.Source
import scala.util.Using
import java.time.Instant
import java.time.format.DateTimeFormatter
import org.json4s.{ Formats, JArray, JValue }
import org.json4s.jackson.JsonMethods.parse

object FileIO {

  type Subscription = (String, String) // (subredditName, url)
  type Post = (String, String, String, String) // (title, selftext, createdUtc, date)

  private def extractString(value: JValue, key: String, formats: Formats): Option[String] =
    (value \ key).extractOpt[String](formats, manifest[String])

  private def extractDouble(value: JValue, key: String, formats: Formats): Option[Double] =
    (value \ key).extractOpt[Double](formats, manifest[Double])

  private def formatEpochSeconds(seconds: Long): String =
    DateTimeFormatter.ISO_INSTANT.format(Instant.ofEpochSecond(seconds))

  // Pure function to read subscriptions from a JSON file
  def readSubscriptions(path: String, formats: Formats): List[Subscription] = {
    val jsonSubscriptions = Using.resource(Source.fromFile(path, "UTF-8")) { src =>
      src.getLines().mkString("\n")
    }

    parse(jsonSubscriptions) match {
      case JArray(values) =>
        values.flatMap { subscription =>
          for {
            name <- extractString(subscription, "name", formats)
            url <- extractString(subscription, "url", formats)
          } yield (name, url)
        }
      case _ => Nil
    }
  }

  // Pure function to download JSON feed from a URL
  def downloadFeed(url: String, formats: Formats): List[Post] = {
    val jsonPosts = Using.resource(Source.fromURL(url)(scala.io.Codec.UTF8)) { src =>
      src.getLines().mkString("\n")
    }

    val children = parse(jsonPosts) \ "data" \ "children"

    children match {
      case JArray(values) =>
        values.flatMap { child =>
          val data = child \ "data"
          for {
            title <- extractString(data, "title", formats)
            createdUtc <- extractDouble(data, "created_utc", formats)
          } yield {
            val selftext = extractString(data, "selftext", formats).getOrElse("")
            val epoch = createdUtc.toLong
            (title, selftext, epoch.toString, formatEpochSeconds(epoch))
          }
        }
      case _ => Nil
    }
  }
}
