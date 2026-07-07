# ratingtables

`ratingtables` is a lightweight, dependency-free R package skeleton for table-driven rating.

It provides:

- long-form factor table helpers
- rating-plan construction
- validation functions
- base-R rating functions
- optional normalized trace output
- trace reshaping helpers
- optional capping helper
- an official base-R demo under `demo/`
- a personal tidyverse-friendly development script under `scripts/`

## Basic local workflow

```r
# From the package root:
# install.packages(c("devtools", "testthat")) # if needed

devtools::document()   # generate man pages from roxygen comments
devtools::test()       # run tests
devtools::install()    # install locally

demo("rating_example", package = "ratingtables")
```

The package functions are written in base R. The script `scripts/dev_rating_example.R` is intentionally ignored by package builds and may use tidyverse packages for table construction.
