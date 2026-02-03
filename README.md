# Albums Challenge

Your challenge is to finish this web app which lists top 100 music albums from iTunes with search and filter functionality.

* Fork this GitHub repository.
* Implement the changes listed below.
* Ensure that the GitHub CI build succeeds.
* Invite [@georgms](https://github.com/georgms) with `Read` access for the review.

## Setup

### Requirements

* `Java 21` or newer

### Running

* `sbt run`
* Open http://localhost:8080 in your browser.

You can also run the app from IntelliJ by running `Application` class.

### Formatting

* Format the code using `sbt scalafmtAll`. This makes it easier for us to perform a code review.

## Your tasks

There is the [test](src/test/scala) directory with tests that cover most of required functionality. You can run tests by
executing `sbt test` or executing them in IntelliJ.

Just like in real life, the requirements as codified in the tests may be ambiguous or incomplete. Use your best
judgement to fill in the gaps and make the app work as you understand the requirements. You may add or alter tests as
you see fit. We will discuss your interpretation of the requirements during the review.

### 1. Implement price and year filtering options.

- Currently, there are some hardcoded filtering options (also called facets) for price and year filters. You need to generate options that are relevant for albums that matched search query.
- Price filtering options should be displayed in ranges, e.g. 0-5, 5-10, 10-15, etc.
- Year options should be all years that match at least one album, in descending order.

### 2. Implement result filtering.

- Search results can be narrowed by selecting some filtering options.
- Filters in the same group should be joined by "OR" and different groups are joined by "AND". For example, if user selects years 2018 and 2017, and price range 5-10, you should show albums which price is in range 5-10 **and** year is 2018 **or** 2017.
- When no filters are selected, show all the albums that match search query.

### 3. Implement count for each filtering option.

- Each filtering option has a count displayed next to it which indicates how many results are matched by the filter. The numbers have to take into account selected filters in other groups and update as user checks or unchecks filters to be accurate for the current filtering combination.
- You should show only the options that will match at least one album. Thus, filtering options might change as user selects other filters. For example, if user selected price range 0-5 and there are no albums that cost less than \$5 and were released in 2017, you shouldn't show year 2017 as a filtering option. But 2017 should appear as filtering option when user selects 5-10 price range (or has no price selected) because there are some albums that were released in 2017 and cost $9.99.
