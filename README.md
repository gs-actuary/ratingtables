# ratingtables

`ratingtables` is a lightweight R package for table-driven rating. It takes policy data, normalized rating tables, and a calculation spec, then returns rated values and optional trace output.

## Motivation

`ratingtables` is designed to address a common problem in insurance premium rating and other table-driven pricing systems: the rating logic is often split across many separate spreadsheets, tabs, formulas, and implementation artifacts. Those formats may be readable to humans, but they are difficult to validate, compare, version, and reproduce programmatically.

This package represents rating factors and additives in a single normalized long-format table. Each row contains one rating value, along with the rate-set metadata, coverage, term name, and variable/level conditions needed to look it up. This structure makes rating tables easier to store, validate, compare, and audit. For human review, helper functions can reshape a rate set back into familiar one-table-per-variable views.

The calculation order is controlled separately by a rating specification. The spec defines which rating terms are applied, in what order, and whether each term is multiplicative, additive, or continuous. This avoids hard-coding repetitive rating formulas into the rater itself: the same engine can rate different products, coverages, rate sets, or scenarios by changing the tables and spec.

The package supports common rating needs such as by-coverage rating, interactions, optional rounding, trace output, driver or entity averaging, and rate capping. The goal is to provide a lightweight rating kernel that actuaries and technical analysts can use for desktop rating, historical re-rating, proposed-rate testing, implementation validation, and reproducible audit trails.

A normalized, executable rating plan can also improve the handoff between pricing and implementation teams. Instead of maintaining separate “desktop raters” and “live raters” with different table structures and duplicated logic, organizations can use `ratingtables` as a reference implementation, a test oracle, or, where appropriate, part of a broader deployment workflow. The package is deliberately storage-agnostic and front-end agnostic: it focuses on making rating logic explicit, testable, and portable.

`ratingtables` is a powerful option for insurance companies and actuarial teams that are trying to modernize their premium raters. Attempts to implement proprietary software can fail or be beset with long delays. These initiatives get sidelined with procurement issues or internal resistance to adopt a new tool. “Low code” GUI environments often have a steep learning curve, and they often lack basic features that are trivial to implement with a flexible coding environment. These are typically inflexible while being no easier to learn than a standard programming language, and they lack the vast online resources of Python, R, or other common languages used in this space. `ratingtables` allows actuarial teams to begin modernizing and standardizing their rating programs immediately without procurement lags and without the need for excessive onboarding with a non-transferable skillset. This premium rater standardization is useful even in cases where an organization is planning to implement a proprietary tool. 

## What it does

`ratingtables` provides:

- long-form factor table helpers
- rating-plan construction
- validation functions
- base-R rating functions
- support for multiplicative, additive, continuous additive, and continuous multiplicative rating terms
- support for one-way factors and multi-variable interactions
- explicit `rate_set_key` selection or automatic rate-set selection by state, charter, book segment, and rating date
- optional normalized trace output
- trace reshaping helpers
- optional capping helpers
- an official base-R demo under `demo/`

The core package functions are implemented in base R, accept ordinary data frames, and avoid prescribing how rating tables are stored, displayed, or deployed.

## Basic local workflow

```r
# From the package root:
# install.packages(c("devtools", "testthat")) # if needed

devtools::document()   # generate man pages from roxygen comments
devtools::test()       # run tests
devtools::install()    # install locally

demo("rating_example", package = "ratingtables")
