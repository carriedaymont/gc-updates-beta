
<!-- README.md is generated from README.Rmd. Please edit that file -->

# growthcleanr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/growthcleanr)](https://cran.r-project.org/package=growthcleanr)
[![R build
status](https://github.com/carriedaymont/gc-updates-beta/workflows/R-CMD-check/badge.svg)](https://github.com/carriedaymont/gc-updates-beta/actions)

<!-- badges: end -->

R package for cleaning height, weight, and head circumference data from
electronic health records.

This beta version is designed for use by experienced users of
growthcleanr.

<a name="cite"></a> This package implements a modified version of the
[Daymont et
al. algorithm](https://academic.oup.com/jamia/article/24/6/1080/3767271),
as specified in Supplemental File 3 within the [Supplementary
Material](https://academic.oup.com/jamia/article/24/6/1080/3767271#97610899)
published with that paper. It includes significant updates that have yet
to be published, but are described in detail in the package
documentation.

> Carrie Daymont, Michelle E Ross, A Russell Localio, Alexander G Fiks,
> Richard C Wasserman, Robert W Grundmeier, Automated identification of
> implausible values in growth data from pediatric electronic health
> records, Journal of the American Medical Informatics Association,
> Volume 24, Issue 6, November 2017, Pages 1080–1087,
> <https://doi.org/10.1093/jamia/ocx037>

This package also includes an R version of the [SAS macro published by
the
CDC](https://www.cdc.gov/nccdphp/dnpao/growthcharts/resources/sas.htm)
for calculating percentiles and Z-scores of pediatric growth
observations and utilities for working with both functions.

## Installation

### Beta (updates) version

This beta is not on CRAN. Install it from GitHub with `remotes`,
building the vignettes so the in-package documentation (start here with
`vignette("start-here-updates-beta")`) is available locally:

``` r
install.packages("remotes")
remotes::install_github("carriedaymont/gc-updates-beta",
                        build_vignettes = TRUE, dependencies = TRUE)
```

`build_vignettes = TRUE` requires the `knitr` and `rmarkdown` packages,
which `dependencies = TRUE` installs for you.
(`pak::pak("carriedaymont/gc-updates-beta")` also works and is faster,
but does not build the vignettes.)

### Stable version

To install the stable version from CRAN:

``` r
install.packages("growthcleanr")
```

## Summary

The `growthcleanr` package processes height, weight, and head
circumference data to identify implausible measurements and other values
that should be excluded from analysis. It uses a variety of techniques,
many of which rely on a comparison of a value to an expected value
derived from the patient’s other values.

Results from `growthcleanr` include a flag for each measurement
indicating a recommendation to include it or exclude it from analysis.
No values are deleted or otherwise removed. These flags can be used
as-is to identify an analytic dataset, but researchers can also examine
detailed results to customize use of growthcleanr. The exclusion flags
also facilitate reporting the reason for excluding a measurement.

To start running `growthcleanr`, an R installation is required, as is a
growth measurement dataset prepared for use in `growthcleanr`.

The rest of the documentation is available as vignettes, also viewable
locally with `browseVignettes("growthcleanr")`:

### Get started:

- [Start
  here](https://carriedaymont.github.io/gc-updates-beta/articles/start-here-updates-beta.html),
  what changed in this beta, installation, a quick start for experienced
  users, and how to give feedback
- [Use of AI assistance in growthcleanr
  development](https://carriedaymont.github.io/gc-updates-beta/articles/ai-role.html),
  how generative AI was and was not used in developing the algorithm,
  software, and documentation

### Configuration and output:

- [Exclusion
  codes](https://carriedaymont.github.io/gc-updates-beta/articles/exclusion-codes.html),
  the exclusion types growthcleanr identifies and how to interpret them
- [Understanding growthcleanr
  output](https://carriedaymont.github.io/gc-updates-beta/articles/output.html),
  every output column and the option that turns it on

### Technical reference:

- [Child
  algorithm](https://carriedaymont.github.io/gc-updates-beta/articles/child-algorithm.html),
  how growthcleanr assesses observations from pediatric subjects
- [Carried-forward value
  rescue](https://carriedaymont.github.io/gc-updates-beta/articles/cf-rescue.html),
  how repeated identical values are detected and when they are rescued
- [Adult
  algorithm](https://carriedaymont.github.io/gc-updates-beta/articles/adult-algorithm.html),
  how growthcleanr assesses observations from adult subjects
- [Z-scores and
  methods](https://carriedaymont.github.io/gc-updates-beta/articles/methods.html),
  the CSD z-score calculation, WHO/CDC blending, and the unit-error
  range
- [Recentering reference
  medians](https://carriedaymont.github.io/gc-updates-beta/articles/recentering.html),
  how the built-in recentering medians were derived
- [The cleangrowth()
  pipeline](https://carriedaymont.github.io/gc-updates-beta/articles/wrapper.html),
  preprocessing, batching, dispatch, and output assembly
- [Configuration
  options](https://carriedaymont.github.io/gc-updates-beta/articles/configuration.html),
  the cross-algorithm parameter and threshold index
- [Computing BMI percentiles and
  Z-scores](https://carriedaymont.github.io/gc-updates-beta/articles/utilities.html),
  utility functions for common data transforms and determining
  percentiles and Z-scores using the CDC method
- [Testing](https://carriedaymont.github.io/gc-updates-beta/articles/testing.html),
  the test suite inventory and how to run it

## Changes

This beta is distributed from the
[`carriedaymont/gc-updates-beta`](https://github.com/carriedaymont/gc-updates-beta)
repository. For a summary of what changed relative to the last CRAN
release, see [Start
here](https://carriedaymont.github.io/gc-updates-beta/articles/start-here-updates-beta.html)
or the
[Changelog](https://carriedaymont.github.io/gc-updates-beta/news/index.html)
(also `NEWS.md`). Earlier tagged releases of growthcleanr are listed [at
GitHub](https://github.com/carriedaymont/growthcleanr/releases).
