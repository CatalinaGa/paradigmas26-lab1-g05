import org.json4s.DefaultFormats

object Main {
  def main(args: Array[String]): Unit = {
    val header = s"Reddit Post Parser\n${"=" * 40}"
    println(header)

    val formats = DefaultFormats
    val subscriptions: List[FileIO.Subscription] = FileIO.readSubscriptions("subscriptions.json", formats)

    val allPosts: List[(String, List[FileIO.Post])] = subscriptions.map { case (name, url) =>
      println(s"Fetching posts from: $url")
      val posts = FileIO.downloadFeed(url, formats)
      val filteredPosts = FileIO.filterPosts(posts).getOrElse(Nil)
      (url, filteredPosts)
    }

    val output = allPosts
      .map { case (url, posts) => Formatters.formatSubscription(url, posts) }
      .mkString("\n")

    println(output)
  }
}
