package albums.challenge.config

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.springframework.context.annotation.{Bean, Configuration}
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder

@Configuration
class JacksonConfig {
  @Bean
  def objectMapper(builder: Jackson2ObjectMapperBuilder): ObjectMapper =
    builder.modules(DefaultScalaModule).build()
}

