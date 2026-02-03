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

    val buckets = defaultPriceBuckets()

    val selectedBuckets = buckets.filter(b => price.contains(b.label))

    val items =
      matched.filter { e =>
        val yearOk = year.isEmpty || year.contains(yearFrom(e.releaseDate))
        val priceOk = price.isEmpty || selectedBuckets.exists(_.contains(e.price))
        yearOk && priceOk
      }

    val priceFacetBase =
      if (year.nonEmpty) matched.filter(e => year.contains(yearFrom(e.releaseDate)))
      else matched

    val yearFacetBase =
      if (price.nonEmpty) matched.filter(e => selectedBuckets.exists(_.contains(e.price)))
      else matched

    Results(
      items = items,
      facets = Map(
        "year" -> yearFacets(yearFacetBase),
        "price" -> priceFacets(priceFacetBase, buckets),
      ),
      query = query,
    )
  }

  private def yearFrom(date: String): String =
    OffsetDateTime.parse(date).getYear.toString

  private def yearFacets(base: List[Entry]): List[Facet] =
    base
      .groupBy(e => yearFrom(e.releaseDate))
      .view
      .mapValues(xs => Integer.valueOf(xs.size))
      .map { case (y, c) => Facet(y, c) }
      .toList
      .sortBy(_.value) // тесты у тебя сейчас ожидают ascending; UI можно и descending

  private def priceFacets(
      base: List[Entry],
      buckets: List[PriceBucket],
  ): List[Facet] =
    buckets
      .map { b =>
        Facet(b.label, Integer.valueOf(base.count(e => b.contains(e.price))))
      }
      .filter(_.count.intValue() > 0)

  private def defaultPriceBuckets(step: Float = 5f, upTo: Float = 25f): List[PriceBucket] = {
    val bounded =
      Iterator
        .iterate(0f)(_ + step)
        .takeWhile(_ < upTo)
        .map(min => PriceBucket(min, Some(min + step)))
        .toList

    bounded :+ PriceBucket(upTo, None)
  }
}
