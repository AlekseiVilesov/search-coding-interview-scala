package albums.challenge.config

import com.fasterxml.jackson.databind.ObjectMapper
import org.springframework.context.annotation.{Bean, Configuration}
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter
import org.springframework.web.client.RestTemplate

@Configuration
class RestTemplateConfig {

  @Bean
  def restTemplate(objectMapper: ObjectMapper): RestTemplate = {
    val rt = new RestTemplate()

    rt.getMessageConverters.forEach {
      case c: MappingJackson2HttpMessageConverter =>
        c.setObjectMapper(objectMapper)
      case _ => ()
    }

    rt
  }
}
