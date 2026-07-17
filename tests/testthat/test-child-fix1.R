# Fix 1 (preterm catch-up rescue) regression fixture.
#
# Locks the gated behavior (Child Steps 11/15/16 catch-up protection with the
# value-level low-tbc gate, tbc.sd <= -1.5) on a labelled sample of 102
# corrected + 100 uncorrected subjects drawn from the synthetic realistic-A
# population. The fixture columns are:
#   base_exclude     - exclusion code WITHOUT Fix 1 (baseline)
#   expected_exclude - exclusion code WITH Fix 1 + gate (current/installed)
#   corr_val         - per-value Corrected / Uncorrected
# See child-fix-specs.md (Fix 1) for the full rationale.

testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

fix1_fixture <- function() {
  f <- system.file("testdata", "child-fix1-catchup-fixture.csv",
                   package = "growthcleanr")
  testthat::skip_if(!nzchar(f), "Fix 1 fixture not installed")
  data.table::fread(f)
}

test_that("Fix 1 catch-up fixture: exclusion codes match expected (gated -1.5)", {
  fx <- fix1_fixture()
  res <- cleangrowth(
    fx[, .(id, subjid, param, agedays, sex, measurement)],
    quietly = TRUE,
    exclude_detail = TRUE
  )
  got <- as.character(res$exclude_detailed)[match(fx$id, res$id)]

  # The fixture records the pre-2026-06-15 internal codes. Map them to the
  # Detailed output level (child is 1:1 except these renames) before comparing.
  exp_detailed <- fx$expected_exclude
  exp_detailed[exp_detailed == "Exclude-C-BIV"]        <- "Exclude-C-Hard-Limit"
  exp_detailed[exp_detailed == "Exclude-C-Identical"]  <- "Exclude-Identical"
  exp_detailed[exp_detailed == "Exclude-C-Extraneous"] <- "Exclude-Extraneous"
  exp_detailed[exp_detailed == "Exclude-Missing"]      <- "Exclude-Missing-Info"
  exp_detailed[exp_detailed == "Exclude-Not-Cleaned"]  <- "Exclude-HC-Out-of-Range"

  # Full behavior lock: every row matches the recorded code.
  expect_equal(got, exp_detailed)

  # NOTE: the former "Fix 1 inert on uncorrected values" cross-check (got[unc]
  # == base_exclude[unc]) was retired 2026-05-23. Correction is now itself
  # corroboration-gated (see .compute corrected-z reversion: form 1 consistent
  # early weight, or form 2 same-day birth length z <= -1.5), so correction STATUS
  # is no longer frozen at the fixture's Fix-1-era corr_val/base_exclude -- form
  # 2 re-corrects real preterms the old reversion had dropped. The full-column
  # lock above covers all rows; corroboration behavior is locked by the
  # dedicated cases in test-child-corroboration.R.
})

test_that("Fix 1 catch-up fixture: key cases behave as designed", {
  fx <- fix1_fixture()
  res <- cleangrowth(
    fx[, .(id, subjid, param, agedays, sex, measurement)],
    quietly = TRUE,
    exclude_detail = TRUE
  )
  code <- function(s, p, a) {
    rid <- fx[subjid == s & param == p & agedays == a]$id[1]
    as.character(res$exclude_detailed[match(rid, res$id)])
  }

  # Genuinely-low preterm catch-up values are rescued (kept).
  expect_equal(code(83453, "WEIGHTKG", 77), "Include")
  expect_equal(code(87905, "HEADCM", 56), "Include")

  # Implausible early values are NOT rescued: the tightened catch-up cap
  # (min(4, 2 + 0.5*ceiling(interval_mo - 1))) and removal of the Step 11
  # first-value (birth) protection now exclude values that sit below birth
  # size even after GA correction (e.g. HC ~24-30 cm in infancy, HT < birth
  # length). These are caught as extreme/moderate trajectory outliers.
  expect_equal(code(99097, "HEADCM", 21), "Exclude-C-Traj")        # 23.9 cm
  expect_equal(code(83453, "HEIGHTCM", 77), "Exclude-C-Traj-Extreme")  # 39.7 cm
  expect_equal(code(84532, "HEADCM", 63), "Exclude-C-Traj-Extreme")   # 30.5 cm

  # Real preterm: low uncorrected z, high corrected z (correction legitimately
  # raises it) -> kept.
  expect_equal(code(86709, "WEIGHTKG", 7), "Include")

  # Correction-inflated non-low value (tbc.sd > -1.5): the low-tbc gate blocks
  # the rescue, so it stays excluded.
  expect_equal(code(75535, "HEADCM", 131), "Exclude-C-Traj")
})
