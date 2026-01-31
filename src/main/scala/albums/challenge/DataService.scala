package albums.challenge

import albums.challenge.models.{Data, Entry}
import org.apache.logging.log4j.{LogManager, Logger}
import org.springframework.cache.annotation.Cacheable
import org.springframework.http.MediaType
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter
import org.springframework.stereotype.Service
import org.springframework.web.client.RestTemplate

import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._

@Service
class DataService(restTemplate: RestTemplate) {
  private val logger: Logger = LogManager.getLogger(classOf[DataService])
  private val uri: String =
    "https://itunes.apple.com/us/rss/topalbums/limit=200/json"

  @Cacheable(Array("entry"))
  def fetch(): List[Entry] = {
    logger.info("Fetching data")

    val converters = restTemplate.getMessageConverters
    converters.asScala.foreach {
      case jsonConverter: MappingJackson2HttpMessageConverter =>
        jsonConverter.setSupportedMediaTypes(
          List(
            new MediaType("application", "json", StandardCharsets.UTF_8),
            new MediaType("text", "javascript", StandardCharsets.UTF_8),
          ).asJava,
        )
      case _ => // Do nothing for other converters
    }

    Option(restTemplate.getForObject(uri, classOf[Data])) match {
      case Some(data) => data.convert()
      case None       => throw new IllegalStateException("Failed to fetch data")
    }
  }
}
