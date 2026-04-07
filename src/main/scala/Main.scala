import org.json4s.DefaultFormats

object Main {
  def main(args: Array[String]): Unit = {
    val header = s"Reddit Post Parser\n${"=" * 40}"
    println(header)

    val formats = DefaultFormats

    // 1. Usamos flatMap para entrar en el Option de suscripciones.
    // Si readSubscriptions devuelve None, todo el bloque se ignora.
    val output = FileIO.readSubscriptions("subscriptions.json", formats).map { subscriptions =>
      
      val allResults = subscriptions.map { case (name, url) =>
        println(s"Fetching posts from: $url")
        
        // 2. Aquí también podemos usar getOrElse(Nil) para que el map 
        // de Main siga teniendo una lista, aunque esté vacía.
        val posts = FileIO.downloadFeed(url, formats).getOrElse(Nil)
        val filteredPosts = FileIO.filterPosts(posts).getOrElse(Nil)
        FileIO.postStats(filteredPosts, url)
        (url, filteredPosts)
      }

      // 3. Formateamos los resultados
      allResults
        .map { case (url, posts) => Formatters.formatSubscription(url, posts) }
        .mkString("\n")
        
    }.getOrElse("No se encontraron suscripciones o el archivo es inválido.")

  }
}