package albums.challenge

import albums.challenge.models.{Entry, Facet, PriceBucket, Results}
import org.springframework.stereotype.Service

import java.time.OffsetDateTime

@Service
class SearchService {
  def search(
      entries: List[Entry],
      query: String,
      year: List[String] = List.empty,
      price: List[String] = List.empty,
  ): Results = {
    val matched =
      entries.filter(_.title.toLowerCase.contains(query.toLowerCase))
    val yearFiltered =
      if (year.nonEmpty)
        matched.filter(e => year.contains(yearFrom(e.releaseDate)))
      else
        matched

    val buckets  = defaultPriceBuckets(step = 5f, upTo = 20f)

    val priceFiltered =
      if (price.nonEmpty) {
        val selectedBuckets =
          buckets.filter(b => price.contains(b.label))

        yearFiltered.filter(e =>
          selectedBuckets.exists(_.contains(e.price))
        )
      } else
        yearFiltered
    Results(
      items = priceFiltered,
      facets = Map("year" -> yearFacets(matched), "price" -> priceFacets(matched)),
      query = ""
    )
  }

  private def yearFrom(date: String): String =
    OffsetDateTime.parse(date).getYear.toString

  private def yearFacets(entries: List[Entry]): List[Facet] =
    entries
      .groupBy(e => yearFrom(e.releaseDate))
      .view
      .mapValues(_.size)
      .map { case (year, count) => Facet(year, count) }
      .toList
      .sortBy(_.value)

  private def priceFacets(entries: List[Entry], buckets: List[PriceBucket] = defaultPriceBuckets()): List[Facet] =
    buckets
      .map { b =>
        Facet(b.label, entries.count(e => b.contains(e.price)))
      }
      .filter(_.count > 0)

  private def defaultPriceBuckets(step: Float = 5f, upTo: Float = 20f): List[PriceBucket] = {
    val bounded =
      Iterator
        .iterate(0f)(_ + step)
        .takeWhile(_ < upTo)
        .map(min => PriceBucket(min, Some(min + step)))
        .toList

    bounded :+ PriceBucket(upTo, None)
  }
}
