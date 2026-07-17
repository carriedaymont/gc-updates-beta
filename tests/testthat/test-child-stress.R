testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Stress test: 400 subjects (8 archetypes × 50) with layered errors
#
# Uses a pre-generated fixture (stress_test_data.rds) containing:
# - 8 growth trajectory archetypes (tracker, falter, catchup, sgaAsym,
#   sgaSym, preterm, latePreterm, PGR)
# - 9 error types independently applied to 10% of patients each
#   (CFs, CF chains, SDE similar, SDE extreme, ±10%, ±50%,
#    unit errors, outlier spikes, swapped HT↔WT)
# - ~33K rows total (31K clean + ~1.3K added SDE rows)
#
# Regenerate fixture with:
#   cd "__Pipeline/error-impact"
#   Rscript "../gc-github-latest/scripts/generate_stress_test_data.R"
#
# Primary purposes:
# 1. Catch algorithm changes via frozen exclusion counts
# 2. Performance benchmark on a realistic, error-laden dataset
# =============================================================================

# ---------------------------------------------------------------------------
# Load fixture
# ---------------------------------------------------------------------------
fixture_path <- file.path(
  testthat::test_path(), "stress_test_data.rds"
)

skip_if(
  !file.exists(fixture_path),
  "Stress test fixture not found. Run scripts/generate_stress_test_data.R from error-impact/ first."
)

fixture <- readRDS(fixture_path)
stress_data <- fixture$data

# ---------------------------------------------------------------------------
# Test 1: Structural invariants
# ---------------------------------------------------------------------------
test_that("stress test: structural invariants hold on 400-subject errored dataset", {

  t0 <- proc.time()
  res <- cleangrowth(
    subjid      = stress_data$subjid,
    param       = stress_data$param,
    agedays     = stress_data$agedays,
    sex         = stress_data$sex,
    measurement = stress_data$measurement,
    id          = stress_data$id,
    quietly     = TRUE,
    exclude_detail = TRUE   # frozen counts (Test 2) are keyed on the Detailed code
  )
  elapsed <- (proc.time() - t0)[["elapsed"]]

  # Report timing
  cat(sprintf(
    "\n=== STRESS BENCHMARK: %d subjects (%d rows) completed in %.1f sec ===\n",
    fixture$n_subjects, nrow(res), elapsed
  ), file = stderr())

  # 1a. Output rows match input
  expect_equal(nrow(res), nrow(stress_data),
               info = "Output row count must match input")

  # 1b. All input IDs present
  expect_true(all(stress_data$id %in% res$id),
              info = "Every input id must appear in output")

  # 1c. No NA exclusion codes
  expect_false(any(is.na(res$exclude)),
               info = "No exclusion codes should be NA")

  # 1d. Valid factor levels
  expect_true(is.factor(res$exclude))

  # 1e. Include + exclusions = total
  counts <- res[, .N, by = exclude]
  expect_equal(sum(counts$N), nrow(stress_data))

  # 1f. Generous performance ceiling (should be ~20-40 sec normally)
  expect_lt(elapsed, 180,
            label = sprintf("Runtime %.1f sec exceeded 180 sec ceiling",
                            elapsed))

  # Save result for subsequent tests
  assign("stress_result", res, envir = parent.env(environment()))
  assign("stress_elapsed", elapsed, envir = parent.env(environment()))
})

# ---------------------------------------------------------------------------
# Test 2: Frozen exclusion counts — update when algorithm changes
# ---------------------------------------------------------------------------
test_that("stress test: exclusion category counts match expected", {

  skip_if(!exists("stress_result"),
          "Skipping: stress_result not available (Test 1 must run first)")

  res <- stress_result
  # Counts are keyed on the Detailed code (exclude_detail = TRUE in Test 1) so
  # the per-step frozen values survive the 2026-06-17 Summary consolidation;
  # the default `exclude` would fold these into the 14 Summary categories.
  catcount <- function(cat) {
    n <- res[exclude_detailed == cat, .N]
    if (length(n) == 0) return(0L)
    return(n)
  }

  # Print current counts for easy reference when updating
  cat("\n--- Stress test exclusion counts ---\n", file = stderr())
  counts <- res[, .N, by = exclude_detailed][order(-N)]
  for (i in seq_len(nrow(counts))) {
    cat(sprintf("  %-40s %5d\n",
                as.character(counts$exclude_detailed[i]), counts$N[i]),
        file = stderr())
  }
  cat("---\n", file = stderr())

  # Total rows
  expect_equal(nrow(res), 33101)

  # Freeze top categories — these are the values to update when the
  # algorithm intentionally changes. Run the test once with new code,
  # read the printed counts, and update here.
  #
  # Frozen counts — updated 2026-04-16: Fenton 2025 CSD z-scores + code rename.
  # Updated 2026-05-22 for the Evil Twins anchor-rule rewrite. Deltas below are
  # the rewrite's actual effect, measured pre-ET vs post-ET on the SAME build
  # (some 2026-04-16 frozen values had also drifted pre-ET via Fix1/Fix3, so the
  # raw old-frozen vs new gap was larger): Evil-Twins 735 -> 72 (~90% fewer);
  # ~96% of former Evil-Twins ceded and re-caught downstream (Traj-Extreme
  # 40 -> 551, Traj 612 -> 739, Abs-Diff 157 -> 169); Include net +20 (39
  # exclude->Include, mostly stable-low/plateau values EWMA does not flag; 19
  # Include->exclude); Too-Many-Errors 385 -> 376. PIV/CF/Identical/Missing
  # unchanged (ET runs after those steps).
  #
  # Updated 2026-05-23 for the Evil Twins selection redesign (windowed median
  # +-4 positions, UER3 unit-error 2-group check, band-of-1), the catch-up cap
  # tighten (min(4, 2 + 0.5*ceiling(interval_mo - 1))), Step 11 birth-protection
  # removal, and the GA-correction corroboration gate (correct only if a
  # consistent early weight OR a same-day birth length z < -2 corroborates
  # prematurity). Cumulative shift from the 2026-05-22 baseline: Include
  # 29349 -> 29363, Evil-Twins 72 -> 66, Traj 739 -> 709, Traj-Extreme
  # 551 -> 524, Too-Many-Errors 376 -> 406, Abs-Diff 169 -> 189, Extraneous
  # 931 -> 930. The correction gate keeps more low-birth-length preterms
  # corrected (net +14 Include) and lets uncorroborated low births fall through.
  # PIV/CF/Identical/Missing unchanged.
  #
  # Updated 2026-05-24 for the Step 15 birth HT/HC regression fix, the form-2
  # corroboration threshold loosen (z < -2 -> <= -1.5), the isolated low
  # uncorrected birth-WT guard, and the robust SDE median.
  # Re-baselined 2026-05-25 for the Step 16 high-birth threshold lowering, the
  # Step 17 birth velocity-adjustment removal, and the robust SDE EWMA drop:
  # Include 29363 -> 29362, Traj 708 -> 709, Abs-Diff 190 -> 191, Unevaluable
  # 406 -> 405 (a couple of relabels); all other categories unchanged.
  # Category names are the Detailed codes (2026-06-17 scheme): the shared
  # Identical/Extraneous/Missing-Info codes drop the `-C-` prefix;
  # the count VALUES are unchanged from the pre-scheme internal literals
  # (Detailed is a 1:1 rename of the child internal codes, no merging).
  # Re-baselined 2026-07-01 for the new CF rescue rules (deltaz/nextz, decided
  # on each string's last CF; lookup retired). 35 CFs rescued: CF 276 -> 241,
  # Counts re-baselined 2026-07-16 for the resmoothed recentering file
  # (inst/extdata/rcfile-resmoothed.csv.gz). Changed from the prior CF-rescue-era
  # baseline: Include 29396 -> 29422, Extraneous 930 -> 928, Evil-Twins 68 -> 64,
  # Traj 715 -> 714, Unevaluable 410 -> 388, Abs-Diff 186 -> 187, Traj-Extreme
  # 517 -> 519 (Hard-Limit/CF/Identical/Missing unchanged; still 11 distinct).
  expect_equal(catcount("Include"), 29422)
  expect_equal(catcount("Exclude-Extraneous"), 928)
  expect_equal(catcount("Exclude-C-Evil-Twins"), 64)
  expect_equal(catcount("Exclude-C-Traj"), 714)
  expect_equal(catcount("Exclude-C-Hard-Limit"), 606)
  expect_equal(catcount("Exclude-C-Unevaluable-Trajectory"), 388)
  expect_equal(catcount("Exclude-C-CF"), 241)
  expect_equal(catcount("Exclude-C-Abs-Diff"), 187)
  expect_equal(catcount("Exclude-C-Traj-Extreme"), 519)
  expect_equal(catcount("Exclude-Identical"), 31)
  expect_equal(catcount("Exclude-Missing-Info"), 1)

  # 11 distinct Detailed categories present (was 18 with param-specific codes)
  expect_equal(
    length(unique(as.character(res$exclude_detailed))), 11
  )
})

# ---------------------------------------------------------------------------
# Test 3: Per-archetype summary
# ---------------------------------------------------------------------------
test_that("stress test: all archetypes processed without crash", {

  skip_if(!exists("stress_result"),
          "Skipping: stress_result not available")

  res <- stress_result

  # Merge archetype info back (coerce to character for join compatibility)
  arch_map <- unique(stress_data[, .(subjid, archetype)])
  arch_map[, subjid := as.character(subjid)]
  res_copy <- copy(res)
  res_copy[, subjid := as.character(subjid)]
  res_arch <- merge(res_copy, arch_map, by = "subjid", all.x = TRUE)

  # Every archetype should have results
  archetypes_present <- unique(res_arch$archetype)
  expect_true(all(fixture$archetypes %in% archetypes_present),
              info = "All 8 archetypes should be in results")

  # Print per-archetype include rates
  cat("\n--- Per-archetype Include rates ---\n", file = stderr())
  arch_summary <- res_arch[, .(
    n = .N,
    n_include = sum(exclude == "Include"),
    pct_include = round(100 * sum(exclude == "Include") / .N, 1)
  ), by = archetype][order(archetype)]

  for (i in seq_len(nrow(arch_summary))) {
    cat(sprintf("  %-15s %5d rows, %5d Include (%5.1f%%)\n",
                arch_summary$archetype[i],
                arch_summary$n[i],
                arch_summary$n_include[i],
                arch_summary$pct_include[i]),
        file = stderr())
  }
  cat("---\n", file = stderr())

  # Each archetype should have some Includes (not all excluded)
  for (arch in fixture$archetypes) {
    arch_inc <- res_arch[archetype == arch & exclude == "Include", .N]
    expect_gt(arch_inc, 0,
              label = sprintf("Archetype '%s' should have some Includes", arch))
  }
})
