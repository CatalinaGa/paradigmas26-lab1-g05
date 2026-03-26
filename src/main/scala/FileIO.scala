import scala.io.Source

object FileIO {
  // Pure function to read subscriptions from a JSON file
  def readSubscriptions(): List[String] = {
    List(
      "https://www.reddit.com/r/scala/.json?count=10",
      "https://www.reddit.com/r/learnprogramming/.json?count=10"
    )
  }

  // Pure function to download JSON feed from a URL
  def downloadFeed(url: String): String = {
    val source = Source.fromURL(url)
    source.mkString
  }
}
