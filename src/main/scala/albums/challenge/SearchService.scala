package albums.challenge

import albums.challenge.models.{Entry, Facet, PriceBucket, Results}
import org.springframework.stereotype.Service

import java.time.OffsetDateTime
import java.util.Locale

@Service
class SearchService {

  private val YearFacetKey = "year"
  private val PriceFacetKey = "price"

  def search(
      entries: List[Entry],
      query: String,
      year: List[String] = List.empty,
      price: List[String] = List.empty,
  ): Results = {

    val q = query.trim.toLowerCase(Locale.ROOT)
    val yearSet = year.toSet
    val priceSet = price.toSet

    val matched =
      if (q.isEmpty) entries
      else entries.filter(e => e.title.toLowerCase(Locale.ROOT).contains(q))

    val buckets = defaultPriceBuckets()
    val selectedBuckets = buckets.filter(b => priceSet.contains(b.label))

    def yearOk(e: Entry): Boolean =
      yearSet.isEmpty || yearSet.contains(yearFrom(e.releaseDate))

    def priceOk(e: Entry): Boolean =
      priceSet.isEmpty || selectedBuckets.exists(_.contains(e.price))

    val items =
      matched.filter(e => yearOk(e) && priceOk(e))

    val priceFacetBase =
      if (yearSet.nonEmpty) matched.filter(e => yearSet.contains(yearFrom(e.releaseDate)))
      else matched

    val yearFacetBase =
      if (priceSet.nonEmpty) matched.filter(e => selectedBuckets.exists(_.contains(e.price)))
      else matched

    Results(
      items = items,
      facets = Map(
        YearFacetKey -> yearFacets(yearFacetBase),
        PriceFacetKey -> priceFacets(priceFacetBase, buckets),
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
      .sortBy(_.value)

  private def priceFacets(
      base: List[Entry],
      buckets: List[PriceBucket],
  ): List[Facet] =
    buckets
      .map(b => Facet(b.label, Integer.valueOf(base.count(e => b.contains(e.price)))))
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
