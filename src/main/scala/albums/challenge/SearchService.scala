package albums.challenge

import albums.challenge.models.{Entry, Facet, PriceBucket, Results}
import org.springframework.stereotype.Service

import java.time.OffsetDateTime
import java.util.Locale

@Service
class SearchService {

  private val YearFacetKey = "year"
  private val PriceFacetKey = "price"

  private val buckets: List[PriceBucket] = defaultPriceBuckets()

  private val bucketsByLabel: Map[String, PriceBucket] =
    buckets.map(b => b.label -> b).toMap

  def search(
      entries: List[Entry],
      query: String,
      year: List[String] = List.empty,
      price: List[String] = List.empty,
  ): Results = {

    val yearSet = year.toSet
    val priceSet = price.toSet

    val matched =
      query.trim match {
        case "" =>
          entries
        case q =>
          val qLower = q.toLowerCase(Locale.ROOT)
          entries.filter(e =>
            e.title.toLowerCase(Locale.ROOT).contains(qLower)
          )
      }

    val selectedBuckets = price.iterator.flatMap(bucketsByLabel.get).toList

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
      .map(e => yearFrom(e.releaseDate))
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .map { case (year, count) => Facet(year, Int.box(count)) }
      .toList
      .sortBy(_.value)

  private def priceFacets(base: List[Entry], buckets: List[PriceBucket]): List[Facet] =
    buckets
      .map { b => Facet(b.label, Int.box(base.count(e => b.contains(e.price)))) }
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
