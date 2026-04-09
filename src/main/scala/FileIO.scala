import scala.io.Source
import scala.util.Using
import org.json4s.{ Formats, JArray, JValue }
import org.json4s.jackson.JsonMethods.parse
import scala.annotation.tailrec


object FileIO {

  type Subscription = (String, String) // (subredditName, url)
  type Post = (String, String, String, String, Int, Int) // (subreddit, title, selftext, date, ups, downs )

  private def extractString(value: JValue, key: String, formats: Formats): Option[String] =
    (value \ key).extractOpt[String](formats, manifest[String])

  private def extractDouble(value: JValue, key: String, formats: Formats): Option[Double] =
    (value \ key).extractOpt[Double](formats, manifest[Double])


private def formatEpochSeconds(seconds: Long): String = {
  // calculo el tiempo base 
  val s = seconds % 60
  val m = (seconds / 60) % 60
  val h = (seconds / 3600) % 24
  val totalDays = (seconds / 86400).toInt

  //cant de dias 
  def esBisiesto(y: Int) = y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)

  @tailrec
  def calcularAño(diasRestantes: Int, añoActual: Int): (Int, Int) = {
    val diasEnEsteAño = if (esBisiesto(añoActual)) 366 else 365
    if (diasRestantes < diasEnEsteAño) (diasRestantes, añoActual)
    else calcularAño(diasRestantes - diasEnEsteAño, añoActual + 1)
  }

  val (diasRestantesDelAño, añoFinal) = calcularAño(totalDays, 1970)

  // meses 
  val leap = esBisiesto(añoFinal)
  val monthDays = List(31, if (leap) 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  // recursividad para encontrar el mes y el día final
  @tailrec
  def calcularMes(dias: Int, meses: List[Int], mesActual: Int): (Int, Int) = {
    if (dias < meses.head) (mesActual + 1, dias + 1)
    else calcularMes(dias - meses.head, meses.tail, mesActual + 1)
  }

  val (mesFinal, diaFinal) = calcularMes(diasRestantesDelAño, monthDays, 0)

  f"$añoFinal%04d-$mesFinal%02d-$diaFinal%02d T$h%02d:$m%02d:$s%02dZ"
}


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
              subreddit  <- extractString(data, "subreddit", formats)
              title      <- extractString(data, "title", formats)
              createdUtc <- extractDouble(data, "created_utc", formats)
              ups        <- extractDouble(data, "ups", formats)
              downs      <- extractDouble(data, "downs", formats)
            } yield {
              val selftext = extractString(data, "selftext", formats).getOrElse("")
              val epoch = createdUtc.toLong
              // Ahora formatEpochSeconds devuelve String, así que calza perfecto en la tupla
              (subreddit, title, selftext, formatEpochSeconds(epoch), ups.toInt, downs.toInt)
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
        posts.filter { case (title, selftext, _, _, _, _) =>
          title.trim.nonEmpty && selftext.trim.nonEmpty
        }  
      )
  }


  def wordsFreq(posts: List[Post]): List[(String, Int)] = {
    
    val words = posts.map(w => w._3.split("\\W+").groupBy(identity _))
    
    val filteredWords = words.map(fw => fw.filter { case (word, _) => word.nonEmpty && word.head.isUpper && !StopWords.stopwords.contains(word.toLowerCase) })

    val wordsOcc = filteredWords.flatMap (wo => wo.map {case (word, array) => (word, array.length) }
    .toList
    .sortBy(_._2).reverse) // Aca cada lista tiene (Word, occurencia) de mayor a menor
    wordsOcc
  }

  def postStats(post: List[Post], url: String): Unit = {

    val score = post.foldLeft(0)((acumulado, p) => acumulado + (p._5 - p._6))
    val CincoPost = post.take(5).map(p => s"""${url} - ${p._2} - ${p._4}""")
    
    println(s"""
    # Estadisticas de los Subreddits
    - Subscription's name: ${post(0)._1}
    - Total Score: $score
    - Most repeated word: ${wordsFreq(post).head}
    - Cinco primeros post: 
    ${CincoPost.mkString("\n  ")}
    """)

  }
}