# ratingtables

`ratingtables` is a lightweight R package for executing table-driven insurance
rating plans. It takes policy or entity data, normalized rating-factor tables,
and an ordered calculation specification, then returns rated values and,
optionally, a step-by-step calculation trace.

The package is intended for actuaries and technical analysts who want rating
logic that is explicit, testable, version-controlled, portable, and open to
ordinary programmatic manipulation.

## Installation

Once `ratingtables` is available on CRAN, install the released version with:

```r
install.packages("ratingtables")
```

The current development version can be installed directly from GitHub:

```r
install.packages("pak")
pak::pkg_install("gs-actuary/ratingtables")
```

Alternatively:

```r
install.packages("remotes")
remotes::install_github("gs-actuary/ratingtables")
```

## Quick start

Load the package and create the included example rating plan:

```r
library(ratingtables)

example <- example_rating_plan()

plan <- example$plan
policies <- example$policies
```

Rate the policies and retain the calculation trace:

```r
result <- rate_policies_with_trace(
  rating_data = policies,
  plan = plan
)
```

Inspect the rated policy data:

```r
result$rated_data
```

Inspect the normalized step-by-step trace:

```r
head(result$term_trace)
```

Explain the calculation for one policy and coverage:

```r
explain_rating(
  rating_result = result,
  row_number = 1,
  coverage = "BI"
)
```

The basic workflow is:

```text
Policy or entity data
          +
Normalized rating-factor tables
          +
Ordered rating specification
          |
          v
      ratingtables
          |
          +---- Rated values
          |
          +---- Step-by-step trace
```

## Motivation

Insurance rating logic is often split across many spreadsheets, workbook tabs,
formulas, rating manuals, implementation documents, system tables, and testing
artifacts. These formats may be readable to humans, but they are difficult to
validate, compare, version, reproduce, and manipulate programmatically.

The same rating algorithm may be represented several times:

- in the actuarial desktop rater;
- in filing or implementation documentation;
- in a production policy system;
- in validation workbooks;
- and in ad hoc testing tools.

Each separate representation creates another opportunity for transcription
errors, inconsistent assumptions, outdated factors, and disagreements over
which implementation is authoritative.

`ratingtables` represents rating factors and additives in a normalized,
long-format table. Each row contains one rating value together with the
rate-set metadata, coverage, term name, and variable-level conditions required
to select it.

The calculation order is represented separately in an explicit rating
specification. The specification identifies which terms are applied, the order
in which they are applied, where their values come from, and how each value
changes the running premium or indicated value.

Separating rating data from calculation order allows the same execution engine
to support different products, coverages, rate sets, states, scenarios, and
proposed-rate revisions without hard-coding a new rater for each one.

## Why an open-source R approach?

Attempts to modernize premium raters through proprietary platforms are often
beset by long delays. Projects can stall in procurement, compete unsuccessfully
for internal technology resources, or encounter resistance to adopting a new
enterprise tool.

Commercial “low-code” rating environments do not necessarily eliminate
technical complexity. They may have steep learning curves, impose rigid
configuration models, and make simple calculations more difficult than they
would be in a flexible programming language. The resulting knowledge is often
specific to one vendor rather than transferable to other analytical or
engineering work.

These environments also lack the broad ecosystem, documentation, community
knowledge, debugging tools, and general-purpose capabilities available in R,
Python, and other widely used programming languages.

`ratingtables` allows an actuarial team to begin modernizing and standardizing
its rating programs immediately:

- without waiting for a procurement cycle;
- without purchasing a proprietary development environment;
- without prolonged onboarding in a non-transferable configuration language;
- and without waiting for a complete production-system implementation.

This standardization remains useful when an organization ultimately plans to
implement a proprietary rating platform. A normalized and executable reference
rater can help define requirements, validate converted rating logic, test a
production implementation, and identify discrepancies between systems.

The package also encourages programmatic retrieval, validation, modification,
display, comparison, and transfer of rating plans. This is a deliberate
contrast with error-prone spreadsheet workflows based on manual copy-and-paste
operations, hidden formulas, duplicated tabs, and repeated reconciliation.

## How it works

A rating workflow has four main components.

### Policy or entity data

Ordinary data frames contain the records to be rated. These may represent
policies, risks, vehicles, drivers, boats, scheduled items, or other rating
entities.

```r
policies
```

### Normalized factor table

Rating factors are stored in a long-format data frame. Each row represents one
factor-table value and the conditions under which it applies.

Conceptually, a factor row contains information such as:

```text
rate-set metadata
coverage
term name
factor value
variable 1 / level 1
variable 2 / level 2
...
```

This format supports both one-way rating factors and multi-variable
interactions.

### Rating specification

The rating specification defines the order of calculation. Each row describes
one rating step, including:

```text
coverage applicability
step number
term name
value source
calculation type
lookup or input information
rounding behavior
```

The factor table answers:

> What value applies?

The rating specification answers:

> When and how is that value used?

### Rating plan

`new_rating_plan()` combines the factor table, rating specification,
coverages, custom functions, and supporting configuration into a validated
rating-plan object.

```r
plan <- new_rating_plan(
  factor_table = factor_table,
  rating_spec = rating_spec,
  coverages = c("BI", "PD")
)
```

The completed plan can then be applied to policy or entity data:

```r
result <- rate_policies_with_trace(
  rating_data = policies,
  plan = plan
)
```

## Key capabilities

`ratingtables` currently provides support for:

- normalized long-form rating-factor tables;
- explicit ordered rating specifications;
- coverage-specific calculation orders;
- exact factor lookup;
- linearly interpolated factor lookup;
- one-way factors and multi-variable interactions;
- multiplicative, additive, and continuous rating steps;
- custom calculation functions;
- explicit or automatic rate-set selection;
- policy-level and entity-level rating;
- generic entity aggregation, including means and sums;
- optional rounding rules;
- step-by-step normalized trace output;
- trace reshaping for human review;
- factor-table and rating-plan validation;
- duplicate factor-key detection;
- premium or rate-change capping helpers.

The core execution functions use base R and accept ordinary data frames. The
package does not prescribe how rating tables must be stored, edited, displayed,
or deployed.

## Use cases

Potential uses include:

### Desktop rating

Build a reproducible alternative to spreadsheet-based desktop raters.

### Historical re-rating

Apply current or proposed rating plans to historical policy records.

### Proposed-rate testing

Compare current and proposed rate sets and analyze premium changes.

### Implementation validation

Use an independent executable rating plan to test a production implementation.

### Reference rating

Maintain an explicit reference representation of the intended rating
algorithm, even when production rating occurs elsewhere.

### Audit and reconciliation

Use calculation traces to identify which terms, factors, and intermediate
values produced a final premium.

### Rating-plan conversion

Convert rating logic from manuals, spreadsheets, system extracts, or other
artifacts into normalized factor tables and an explicit calculation
specification.

The package executes the completed rating plan. Extracting and interpreting
rating logic from arbitrary source materials remains a separate workflow that
may require actuarial judgment.

## Trace output

Traceability is a central design goal.

Rather than returning only the final rated value, the package can retain one
record for each calculation step. A trace can show:

```text
policy or row identifier
coverage
step number
term name
value source
input value
looked-up or calculated value
value before the step
value after the step
factor-table row used
interpolation details, where applicable
```

This makes the calculation easier to inspect, validate, compare, and explain.

```r
result <- rate_policies_with_trace(
  rating_data = policies,
  plan = plan
)

trace_to_excel_style(result$term_trace)
```

The trace can also be reshaped into wide factor columns:

```r
trace_to_wide_factors(result$term_trace)
```

## Entity rating and aggregation

Some rating values originate below the policy level.

Examples include:

- multiple drivers whose factors are averaged;
- boats whose premiums are summed;
- scheduled items whose premiums are summed;
- multiple vehicles or locations;
- endorsements attached to a parent policy.

The generic entity workflow is:

```text
Entity records
      |
      v
Rate entity rows
      |
      v
Aggregate by parent
      |
      v
Join aggregated values to parent records
      |
      v
Execute parent rating plan
```

The principal functions are:

```r
rate_entities()
aggregate_entity_values()
join_entity_values()
```

The same framework can support different entity types without requiring
product-specific functions in the core package.

## Demos and larger examples

Run the installed package demo with:

```r
demo("rating_example", package = "ratingtables")
```

Additional development examples are available in the repository's `scripts/`
directory. These scripts are retained in the GitHub repository for learning and
demonstration but are excluded from the built package.

The README quick start is intentionally small. The longer examples are intended
to demonstrate more realistic rating workflows, interactions, interpolation,
entity aggregation, custom calculations, and trace review.

## Project status

`ratingtables` is under active development.

The core rating-plan, lookup, rating, entity-aggregation, validation, and trace
workflows are functional. The public API may continue to evolve before version
1.0.0 as the package is tested against additional real-world rating structures.

Feedback from actuaries, pricing analysts, implementation teams, and other
potential users is welcome, particularly regarding:

- rating structures that are difficult to represent;
- trace and audit requirements;
- implementation-validation workflows;
- entity-level rating needs;
- interpolation and calculation-order behavior;
- usability of the factor-table and specification formats.

## Contributing

Bug reports, reproducible examples, feature requests, documentation
improvements, and focused pull requests are welcome.

See the
[contribution guidelines](https://github.com/gs-actuary/ratingtables/blob/master/CONTRIBUTING.md)
for project principles and contribution guidelines.

Large design changes should be discussed in a GitHub issue before
implementation. The package aims to remain lightweight, transparent,
storage-agnostic, and based on generic rating concepts rather than
product-specific hard-coded functions.

## Development

The following commands are useful when developing the package from a local
checkout:

```r
devtools::document()
devtools::test()
devtools::check()
devtools::install()
```

These commands are for package development. Ordinary users do not need to clone
the repository or run the development workflow to install and use the package.

## License

`ratingtables` is released under the MIT License.