package albums.challenge

import albums.challenge.models.{Entry, Facet, Results}
import munit.FunSuite

class SearchServiceTest extends FunSuite {

  private val searchService = new SearchService()

  private val YearFacetKey = "year"
  private val PriceFacetKey = "price"

  private val entry2002_9_99 = anEntry(
    title = "Legend: The Best of Bob Marley and the Wailers (Remastered)",
    price = 9.99f,
    releaseDate = "2002-01-01T00:00:00-07:00",
  )

  private val entry2008_19_99 = anEntry(
    title = "The Very Best of The Doors",
    price = 19.99f,
    releaseDate = "2008-01-29T00:00:00-07:00",
  )

  private val entry1978_5 = anEntry(
    title = "The Best of Earth Wind & Fire Vol. 1",
    price = 5f,
    releaseDate = "1978-11-23T00:00:00-07:00",
  )

  private val entry1994_15_99 = anEntry(
    title = "The Least Worst of Type O Negative",
    price = 15.99f,
    releaseDate = "1994-11-13T00:00:00-07:00",
  )

  private val entry1983_20 = anEntry(
    title = "The Best of Sade",
    price = 20f,
    releaseDate = "1983-01-01T00:00:00-07:00",
  )

  private val allEntries =
    List(entry2002_9_99, entry2008_19_99, entry1978_5, entry1994_15_99, entry1983_20)

  private val matchesForQueryBest =
    List(entry2002_9_99, entry2008_19_99, entry1978_5, entry1983_20)

  test("search: returns all entries when query is empty") {
    val result = searchService.search(
      entries = allEntries,
      query = "",
      year = Nil,
      price = Nil,
    )

    assertEquals(result.items, allEntries)
  }

  test("search: returns matching entries when query matches general keyword") {
    val result = searchService.search(
      entries = allEntries,
      query = "best",
      year = Nil,
      price = Nil,
    )

    assertEquals(result.items, matchesForQueryBest)
  }

  test("search: returns only that entry when query matches exact keyword") {
    val result = searchService.search(
      entries = allEntries,
      query = "doors",
      year = Nil,
      price = Nil,
    )

    assertEquals(result.items, List(entry2008_19_99))
  }

  test("facets: generates price buckets from query matches") {
    val result = searchService.search(
      entries = allEntries,
      query = "best",
      year = Nil,
      price = Nil,
    )

    assertEquals(
      result.facets.get(PriceFacetKey),
      Option(
        List(
          Facet("5 - 10", 2),
          Facet("15 - 20", 1),
          Facet("20 - 25", 1),
        ),
      ),
    )
  }

  test("facets: generates years from query matches (ascending)") {
    val result = searchService.search(
      entries = allEntries,
      query = "best",
      year = Nil,
      price = Nil,
    )

    assertEquals(
      result.facets.get(YearFacetKey),
      Option(
        List(
          Facet("1978", 1),
          Facet("1983", 1),
          Facet("2002", 1),
          Facet("2008", 1),
        ),
      ),
    )
  }

  test("filters: selecting multiple years uses OR within year group") {
    val selectedYears = List("2002", "2008")

    val result = searchService.search(
      entries = allEntries,
      query = "best",
      year = selectedYears,
      price = Nil,
    )

    assertEquals(
      result.items,
      List(entry2002_9_99, entry2008_19_99),
      clue = "items (OR within year group)",
    )

    assertEquals(
      result.facets.get(YearFacetKey),
      Some(
        List(
          Facet("1978", 1),
          Facet("1983", 1),
          Facet("2002", 1),
          Facet("2008", 1),
        ),
      ),
      clue = "year facets (should remain available for multi-select)",
    )

    assertEquals(
      result.facets.get(PriceFacetKey),
      Some(
        List(
          Facet("5 - 10", 1),
          Facet("15 - 20", 1),
        ),
      ),
      clue = "price facets (filtered by selected years)",
    )
  }

  test("filters: selecting year and price uses AND between facet groups") {
    val selectedYears = List("2002")
    val selectedPrices = List("5 - 10")

    val result = searchService.search(
      entries = allEntries,
      query = "best",
      year = selectedYears,
      price = selectedPrices,
    )

    assertEquals(
      result.items,
      List(entry2002_9_99),
      clue = "items (AND between year and price groups)",
    )

    assertEquals(
      result.facets.get(YearFacetKey),
      Some(
        List(
          Facet("1978", 1),
          Facet("2002", 1),
        ),
      ),
      clue = "year facets (filtered by selected price)",
    )

    assertEquals(
      result.facets.get(PriceFacetKey),
      Some(List(Facet("5 - 10", 1))),
      clue = "price facets (filtered by selected year)",
    )
  }

  test("filters: facet options update based on other selected filters (no zero-count options)") {
    val selectedYears = List("2002", "2008")
    val selectedPrices = List("15 - 20")

    val result = searchService.search(
      entries = allEntries,
      query = "best",
      year = selectedYears,
      price = selectedPrices,
    )

    assertEquals(
      result.items,
      List(entry2008_19_99),
      clue = "items (year AND price)",
    )

    assertEquals(
      result.facets.get(YearFacetKey),
      Some(List(Facet("2008", 1))),
      clue = "year facets (no zero-count options)",
    )

    assertEquals(
      result.facets.get(PriceFacetKey),
      Some(
        List(
          Facet("5 - 10", 1),
          Facet("15 - 20", 1),
        ),
      ),
      clue = "price facets (no zero-count options)",
    )
  }

  test("ux: year facets remain available after selecting a year") {
    val selectedYears = List("2002")

    val result = searchService.search(
      entries = allEntries,
      query = "best",
      year = selectedYears,
      price = Nil,
    )

    assertEquals(result.items, List(entry2002_9_99))
    assertEquals(
      result.facets.get(YearFacetKey),
      Some(
        List(
          Facet("1978", 1),
          Facet("1983", 1),
          Facet("2002", 1),
          Facet("2008", 1),
        ),
      ),
    )
  }

  test("filters: price bucket boundary - 10.0 is not in 5 - 10") {
    val boundary = anEntry(
      title = "Boundary",
      price = 10.0f,
      releaseDate = "2005-01-01T00:00:00-07:00",
    )

    val result = searchService.search(
      entries = List(boundary),
      query = "",
      year = Nil,
      price = List("5 - 10"),
    )

    assertEquals(
      result.items,
      Nil,
      clue = "10.0 should not match '5 - 10' bucket (boundary belongs to next bucket)",
    )
  }

  private def anEntry(title: String, price: Float, releaseDate: String): Entry =
    Entry(
      title = title,
      price = price,
      releaseDate = releaseDate,
      link = "",
      image = "",
    )
}
