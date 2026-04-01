import scala.io.Source
import scala.util.Using
import java.time.Instant
import java.time.format.DateTimeFormatter
import org.json4s.{ Formats, JArray, JValue }
import org.json4s.jackson.JsonMethods.parse

object FileIO {

  type Subscription = (String, String) // (subredditName, url)
  type Post = (String, String, String, String) // (subreddit, title, selftext, date)

  private def extractString(value: JValue, key: String, formats: Formats): Option[String] =
    (value \ key).extractOpt[String](formats, manifest[String])

  private def extractDouble(value: JValue, key: String, formats: Formats): Option[Double] =
    (value \ key).extractOpt[Double](formats, manifest[Double])

  private def formatEpochSeconds(seconds: Long): String =
    DateTimeFormatter.ISO_INSTANT.format(Instant.ofEpochSecond(seconds))

  // Pure function to read subscriptions from a JSON file
  def readSubscriptions(path: String, formats: Formats): Option[List[Subscription]] = {
    try{
        val jsonSubscriptions = Using.resource(Source.fromFile(path, "UTF-8")) { src =>
        src.getLines().mkString("\n")
      }

      
      val subs = parse(jsonSubscriptions) match {
        case JArray(values) =>
          values.flatMap { subscription =>
            for {
              name <- extractString(subscription, "name", formats)
              url  <- extractString(subscription, "url", formats)
            } yield (name, url)
          }
        case _ => Nil
      }
      if (subs.isEmpty) None else Some(subs)

    }catch{
       case _ : Exception =>None
    }

  }

  // Pure function to download JSON feed from a URL
  def downloadFeed(url: String, formats: Formats): Option[List[Post]] = {
    try{ 
      
      val jsonPosts = Using.resource(Source.fromURL(url)(scala.io.Codec.UTF8)) { src =>
        src.getLines().mkString("\n")
      }
      
      val children = parse(jsonPosts) \ "data" \ "children"

      val allPosts = children match {
        case JArray(values) =>
          values.flatMap { child =>
            val data = child \ "data"
            for {
              subreddit <- extractString(data, "subreddit", formats)
              title     <- extractString(data, "title", formats)
              createdUtc <- extractDouble(data, "created_utc", formats)
            } yield {
              val selftext = extractString(data, "selftext", formats).getOrElse("")
              val epoch = createdUtc.toLong
              // Ahora formatEpochSeconds devuelve String, así que calza perfecto en la tupla
              (subreddit, title, selftext, formatEpochSeconds(epoch))
            }
          }
        case _ => Nil
      }
      if (allPosts.isEmpty) None else Some(allPosts)
    }catch{
      case _ : Exception => None 
    }

    
  }

  def filterPosts(posts: List[Post]): Option[List[Post]] = {
      Some (
        posts.filter { case (title, selftext, _, _) =>
          title.trim.nonEmpty && selftext.trim.nonEmpty
        }  
      )
  }


  def wordsFreq(posts: List[Post]): List[Map[String, Int]] = {
    
    val stopwords = Set("the", "about", "above", "after", "again", "against", "all", "am", "an",
    "and", "any", "are", "aren't", "as", "at", "be", "because", "been",
    "before", "being", "below", "between", "both", "but", "by", "can't",
    "cannot", "could", "couldn't", "did", "didn't", "do", "does", "doesn't",
    "doing", "don't", "down", "during", "each", "few", "for", "from", "further",
    "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd",
    "he'll", "he's", "her", "here", "here's", "hers", "herself", "him",
    "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if",
    "in", "into", "is", "isn't", "it", "it's", "its", "itself", "let's", "me",
    "more", "most", "mustn't", "my", "myself", "no", "nor", "not", "of", "off",
    "on", "once", "only", "or", "other", "ought", "our", "ours", "ourselves",
    "out", "over", "own", "same", "shan't", "she", "she'd", "she'll", "she's",
    "should", "shouldn't", "so", "some", "such", "than", "that", "that's",
    "the", "their", "theirs", "them", "themselves", "then", "there", "there's",
    "these", "they", "they'd", "they'll", "re", "they've", "this", "those",
    "through", "to", "too", "under", "until", "up", "very", "was", "wasn't",
    "we", "we'd", "we'll", "we're", "we've", "were", "weren't", "what",
    "what's", "when", "when's", "where", "where's", "which", "while", "who",
    "who's", "whom", "why", "why's", "with", "won't", "would",
    "wouldn't", "you", "you'd", "you'll", "you're", "you've", "your", "yours",
    "yourself", "yourselves")
    
    val words = posts.map(p => p._3.split("\\W+").groupBy(identity _))
    
    val filteredWords = words.map(m => m.filter { case (word, _) => word.nonEmpty && word.head.isUpper && !stopwords.contains(word.toLowerCase) })

    val wordsOcc = filteredWords.map (s => s.map {case (word, array) => (word, array.length) }) // Aca cada lista tiene (Word -> occurencia)

    wordsOcc
  }
}
