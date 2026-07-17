testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Child Step 7 PIV constructed tests (Tier 1 testing-gaps: T18, T19).
#
# Reported Summary code for any PIV is "Exclude-Hard-Limit" (population implausible
# value; internal literal "Exclude-C-PIV", Detailed "Exclude-C-Hard-Limit"). These
# tests assert the default `exclude` (Summary) column.
#
# PIV uses the unrecentered WHO/CDC-blended CSD z-score (sd.orig_uncorr). Per
# feedback_run_dont_derive, every constructed subject was run through
# cleangrowth() and the z-scores/codes confirmed before freezing here.
# =============================================================================

run_excl <- function(d, ...) {
  res <- as.data.table(suppressWarnings(
    cleangrowth(d, quietly = TRUE, ...)))
  as.character(res$exclude)
}

# ---------------------------------------------------------------------------
# T18 — Standardized-PIV low-side cutoffs + the 1-year split.
#
# Tested on HEIGHTCM. (Weight has no standardized low cutoff — removed
# 2026-06-25 — because the low-WT z asymptotes near -10 and can never reach a
# useful threshold; HT/HC low z's ARE reachable just above their absolute
# floors, so HT exercises the split on the SHIPPED defaults.)
#
# A stably-low HT trajectory holds every value at z ~ -19 (flat in z, so
# trajectory steps never fire), which sits strictly between the default
# cutoffs (piv.z.ht.low.old = -15, piv.z.ht.low.young = -25):
#   ages 200, 300  -> age < 1y  -> governed by piv.z.ht.low.young (-25): Include
#   ages 450, 550  -> age >= 1y -> governed by piv.z.ht.low.old  (-15): PIV
# ---------------------------------------------------------------------------
piv_lo <- data.table(
  subjid = "lo", param = "HEIGHTCM", sex = 0L,
  agedays = c(200L, 300L, 450L, 550L),
  measurement = c(27.0, 29.0, 30.5, 30.0),   # all z ~ -19 (confirmed)
  id = 1:4
)

test_that("default HT low cutoffs (-25/-15) split a z~-19 trajectory at 1y", {
  # The >=1y values are excluded as PIV though they are NOT more extreme than
  # the <1y values (all z ~ -19) — purely the age split acting on the defaults.
  expect_equal(run_excl(piv_lo),
               c("Include", "Include", "Exclude-Hard-Limit", "Exclude-Hard-Limit"))
})

test_that("piv.z.ht.low.old is plumbed (loosening it rescues the >=1y values)", {
  expect_equal(run_excl(piv_lo, piv.z.ht.low.old = -25), rep("Include", 4))
})

test_that("piv.z.ht.low.young is plumbed (tightening it excludes the <1y values)", {
  expect_equal(run_excl(piv_lo, piv.z.ht.low.young = -15), rep("Exclude-Hard-Limit", 4))
})

# ---------------------------------------------------------------------------
# T19 — Absolute PIV birth-specific backstops.
#
# At agedays == 0 the absolute caps tighten: WT > 10.5, HT > 65, HC > 50 are
# each PIV. The same magnitude at a non-birth age is NOT subject to the birth
# cap, isolating the `agedays == 0` guard.
# ---------------------------------------------------------------------------
test_that("birth WT > 10.5 is PIV; the same 12 kg at a non-birth age is Include", {
  d <- data.table(
    subjid = "bw", param = "WEIGHTKG", sex = 0L,
    agedays = c(0L, 180L, 365L, 730L),
    measurement = c(12.0, 8.0, 10.0, 12.0),  # 12 at birth and again at 730d
    id = 1:4
  )
  expect_equal(run_excl(d),
               c("Exclude-Hard-Limit", "Include", "Include", "Include"))
})

test_that("birth HT > 65 is PIV; non-birth heights > 65 are Include", {
  d <- data.table(
    subjid = "bh", param = "HEIGHTCM", sex = 0L,
    agedays = c(0L, 180L, 365L, 730L),
    measurement = c(70.0, 67.0, 75.0, 85.0),  # 75 and 85 (>65) are non-birth
    id = 1:4
  )
  expect_equal(run_excl(d),
               c("Exclude-Hard-Limit", "Include", "Include", "Include"))
})

test_that("birth HC > 50 is PIV; the same 52 cm at a non-birth age is Include", {
  # HC needs a HT companion (its DOP) and stays within the 3y HC cleaning range.
  d <- rbind(
    data.table(subjid = "bc", param = "HEADCM", sex = 0L,
               agedays = c(0L, 180L, 365L, 730L),
               measurement = c(52.0, 45.0, 48.0, 52.0), id = 1:4),
    data.table(subjid = "bc", param = "HEIGHTCM", sex = 0L,
               agedays = c(0L, 180L, 365L, 730L),
               measurement = c(50.0, 67.0, 77.0, 87.0), id = 5:8)
  )
  res <- as.data.table(suppressWarnings(cleangrowth(d, quietly = TRUE)))
  hc <- res[param == "HEADCM"][order(agedays)]
  expect_equal(as.character(hc$exclude),
               c("Exclude-Hard-Limit", "Include", "Include", "Include"))
})

test_that("WT > 35 under 2y is PIV; the same 40 kg at >2y is not PIV", {
  # Offender: 40 kg at 400 days (<2y), surrounded by normal weights.
  offender <- data.table(
    subjid = "wa", param = "WEIGHTKG", sex = 0L,
    agedays = c(200L, 400L, 600L), measurement = c(10.0, 40.0, 12.0), id = 1:3
  )
  expect_equal(run_excl(offender), c("Include", "Exclude-Hard-Limit", "Include"))

  # Same magnitude at >2y: the <2y cap no longer applies, so a stable ~40 kg
  # trajectory is never flagged PIV (no Exclude-Hard-Limit anywhere).
  control <- data.table(
    subjid = "wb", param = "WEIGHTKG", sex = 0L,
    agedays = c(800L, 900L, 1000L, 1100L),
    measurement = c(40.0, 40.5, 41.0, 41.5), id = 1:4
  )
  out <- run_excl(control)
  expect_false(any(out == "Exclude-Hard-Limit"))
  expect_equal(out[1], "Include")
})
