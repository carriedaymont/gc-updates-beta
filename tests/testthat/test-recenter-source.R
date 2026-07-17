# recenter_source wiring for cleangrowth() and cleangrowth_checkpoint().
#
# Tests the PLUMBING of recenter_source ("reference" vs "none"), supplied
# sd.recenter precedence, and the recenter_detail output contract.

testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# Small independent-subject dataset with plausible HT/WT so z-scores compute.
make_small <- function(n_subj = 12) {
  ages <- c(0L, 180L, 365L, 730L, 1460L)
  rbindlist(lapply(seq_len(n_subj), function(s) {
    off <- (s %% 5) * 0.1
    data.table(
      subjid      = s,
      param       = rep(c("WEIGHTKG", "HEIGHTCM"), each = length(ages)),
      agedays     = rep(ages, 2),
      sex         = s %% 2L,
      measurement = c(3.3 + off + c(0, 3.5, 6.5, 9.0, 12.5),   # WEIGHTKG
                      50  + off + c(0, 15,  25,  33,  50)),     # HEIGHTCM
      id          = NA_integer_
    )
  }))[, id := .I][]
}

# ---------------------------------------------------------------------------
# cleangrowth(): reference path is unchanged; none path disables recentering
# ---------------------------------------------------------------------------

test_that('recenter_source = "reference" equals the default (byte-identical)', {
  d <- make_small()
  r_def <- cleangrowth(d, quietly = TRUE, debug = TRUE)
  r_ref <- cleangrowth(d, recenter_source = "reference", quietly = TRUE, debug = TRUE)
  expect_identical(as.character(r_def$exclude), as.character(r_ref$exclude))
  expect_equal(r_def$tbc.sd, r_ref$tbc.sd)
})

test_that('recenter_source = "none" disables recentering (tbc.sd == sd.orig)', {
  d <- make_small()
  r <- suppressMessages(cleangrowth(d, recenter_source = "none", quietly = TRUE, debug = TRUE))
  expect_equal(nrow(r), nrow(d))
  # child rows: the recentered z equals the reference z (no median subtracted)
  ok <- !is.na(r$tbc.sd) & !is.na(r$sd.orig)
  expect_true(any(ok))
  expect_equal(r$tbc.sd[ok], r$sd.orig[ok])
  # and it differs from the recentered default somewhere (recentering does something)
  r_def <- cleangrowth(d, quietly = TRUE, debug = TRUE)
  expect_false(isTRUE(all.equal(r$tbc.sd, r_def$tbc.sd)))
})

test_that("invalid recenter_source errors", {
  d <- make_small()
  expect_error(cleangrowth(d, recenter_source = "bogus", quietly = TRUE),
               "recenter_source")
  expect_error(cleangrowth(d, recenter_source = c("reference", "none"), quietly = TRUE),
               "recenter_source")
})

test_that("a supplied sd.recenter table wins over recenter_source", {
  d <- make_small()
  # the built-in reference used as an explicit supplied table must be used
  # verbatim regardless of the recenter_source value.
  rc <- fread(system.file("extdata", "rcfile-resmoothed.csv.gz", package = "growthcleanr"))
  r_a <- cleangrowth(d, sd.recenter = copy(rc), recenter_source = "none",
                     quietly = TRUE, debug = TRUE)
  r_b <- cleangrowth(d, sd.recenter = copy(rc), recenter_source = "reference",
                     quietly = TRUE, debug = TRUE)
  expect_identical(as.character(r_a$exclude), as.character(r_b$exclude))
  expect_equal(r_a$tbc.sd, r_b$tbc.sd)
})

# ---------------------------------------------------------------------------
# cleangrowth_checkpoint(): recenter_source forwarded to each chunk unchanged
# ---------------------------------------------------------------------------

test_that('checkpoint recenter_source = "reference" matches direct cleangrowth', {
  d   <- make_small()
  dir <- file.path(tempdir(), "gc_ckpt_ref")
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  res <- cleangrowth_checkpoint(d, dir, chunk_size = 4, quietly = TRUE,
                                recenter_source = "reference")
  ref <- cleangrowth(d, quietly = TRUE)
  m <- merge(res[, .(id, exclude)], ref[, .(id, exclude_ref = exclude)], by = "id")
  expect_equal(as.character(m$exclude), as.character(m$exclude_ref))
})

test_that('checkpoint recenter_source = "none": chunked == unchunked', {
  d   <- make_small()
  dir <- file.path(tempdir(), "gc_ckpt_none")
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  res <- suppressMessages(cleangrowth_checkpoint(d, dir, chunk_size = 4, quietly = TRUE,
                                                 recenter_source = "none"))
  ref <- suppressMessages(cleangrowth(d, recenter_source = "none", quietly = TRUE))
  m <- merge(res[, .(id, exclude)], ref[, .(id, exclude_ref = exclude)], by = "id")
  expect_equal(as.character(m$exclude), as.character(m$exclude_ref))
})

test_that("checkpoint invalid recenter_source errors", {
  d   <- make_small()
  dir <- file.path(tempdir(), "gc_ckpt_bad")
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  expect_error(cleangrowth_checkpoint(d, dir, chunk_size = 4, quietly = TRUE,
                                      recenter_source = "derive"),
               "recenter_source")
})

# ---------------------------------------------------------------------------
# recenter_detail: stable, named output contract for the recentered CSD z
# ---------------------------------------------------------------------------

rc_detail_cols <- c("tbc.sd", "ctbc.sd", "rcmedian.sd")

test_that("recenter_detail is off by default (columns absent)", {
  d <- make_small()
  r <- cleangrowth(d, quietly = TRUE)
  expect_false(any(rc_detail_cols %in% names(r)))
})

test_that("recenter_detail = TRUE exposes tbc.sd / ctbc.sd / rcmedian.sd", {
  d <- make_small()
  r <- cleangrowth(d, recenter_detail = TRUE, quietly = TRUE)
  expect_true(all(rc_detail_cols %in% names(r)))
  # child rows carry real recentered z-scores (not all NA)
  expect_false(all(is.na(r$tbc.sd)))
  expect_false(all(is.na(r$rcmedian.sd)))
  # the contract columns are byte-identical to what debug surfaces
  r_dbg <- cleangrowth(d, debug = TRUE, quietly = TRUE)
  expect_equal(r$tbc.sd,  r_dbg$tbc.sd)
  expect_equal(r$ctbc.sd, r_dbg$ctbc.sd)
  # recentering identity: tbc.sd == sd.orig - rcmedian.sd (sd.orig from debug)
  expect_equal(r$tbc.sd, r_dbg$sd.orig - r$rcmedian.sd)
})

test_that("full_detail turns on recenter_detail", {
  d <- make_small()
  r <- cleangrowth(d, full_detail = TRUE, quietly = TRUE)
  expect_true(all(rc_detail_cols %in% names(r)))
})

test_that("recenter_detail + debug together produce no duplicate columns", {
  d <- make_small()
  r <- cleangrowth(d, recenter_detail = TRUE, debug = TRUE, quietly = TRUE)
  expect_false(any(duplicated(names(r))))
  # each contract column appears exactly once
  for (cc in rc_detail_cols) expect_equal(sum(names(r) == cc), 1L)
})

test_that("checkpoint forwards recenter_detail to each chunk", {
  d   <- make_small()
  dir <- file.path(tempdir(), "gc_ckpt_rcdetail")
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  res <- cleangrowth_checkpoint(d, dir, chunk_size = 4, quietly = TRUE,
                                recenter_detail = TRUE)
  expect_true(all(rc_detail_cols %in% names(res)))
  expect_false(all(is.na(res$tbc.sd)))
})

# ---------------------------------------------------------------------------
# Scale regression guard: reference path byte-identical to today on real data
# ---------------------------------------------------------------------------

test_that('recenter_source = "reference" is byte-identical to the default on syngrowth', {
  # Realistic subset (HT/WT/HC, potcorr, adult rows) so the guard exercises the
  # full pipeline, not just constructed data.
  data("syngrowth", package = "growthcleanr", envir = environment())
  sg <- as.data.table(syngrowth)
  subjs <- unique(sg$subjid)[1:300]
  d <- sg[subjid %in% subjs]

  r_def <- cleangrowth(d, quietly = TRUE, recenter_detail = TRUE)
  r_ref <- cleangrowth(d, quietly = TRUE, recenter_detail = TRUE,
                       recenter_source = "reference")
  expect_identical(as.character(r_def$exclude), as.character(r_ref$exclude))
  expect_identical(r_def$tbc.sd,      r_ref$tbc.sd)
  expect_identical(r_def$ctbc.sd,     r_ref$ctbc.sd)
  expect_identical(r_def$rcmedian.sd, r_ref$rcmedian.sd)
})
