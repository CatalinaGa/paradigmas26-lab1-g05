
object Formatters {

  // Pure function to format posts from a subscription
  def formatSubscription(url: String, posts: List[FileIO.Post]): String = {
    val header = s"\n${"=" * 80}\nPosts from: $url \n${"=" * 80}"
    val formattedPosts = posts.map { case (subreddit, title, selftext, date) =>
      s"-> [$subreddit] $title ($date)"
    }.mkString("\n")

    header + "\n" + formattedPosts
  }
}
