# cleangrowth_checkpoint(): fresh-by-default, resume opt-in, and the chunk cap.

testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

make_mini <- function() {
  ages <- c(0L, 180L, 365L)
  d <- data.table::rbindlist(lapply(1:6, function(s) {
    data.table::data.table(
      subjid      = s,
      param       = rep(c("WEIGHTKG", "HEIGHTCM"), each = length(ages)),
      agedays     = rep(ages, 2),
      sex         = s %% 2L,
      measurement = c(3.3 + 0.01 * s + c(0, 5.5, 7.0),
                      50  + 0.10 * s + c(0, 18, 25))
    )
  }))
  d[, id := .I]
  d[]
}

test_that("default run starts fresh: stale checkpoints removed, result correct", {
  d   <- make_mini()
  dir <- file.path(tempdir(), "gc_ckpt_fresh")
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  # leftovers from a prior run: a same-numbered bogus file + a higher stale one
  saveRDS(data.frame(bogus = 1), file.path(dir, "chunk_0001.rds"))
  saveRDS(data.frame(bogus = 1), file.path(dir, "chunk_0099.rds"))

  res <- cleangrowth_checkpoint(d, dir, chunk_size = 3, quietly = TRUE)

  # stale higher-numbered file is gone (cannot leak into the combine)
  expect_false(file.exists(file.path(dir, "chunk_0099.rds")))
  # every input row is present
  expect_equal(nrow(res), nrow(d))
  # matches a direct cleangrowth() run, per id (subjects are independent, so
  # chunking must not change any result)
  ref <- cleangrowth(d, quietly = TRUE)
  m <- merge(res[, .(id, exclude)], ref[, .(id, exclude_ref = exclude)], by = "id")
  expect_equal(as.character(m$exclude), as.character(m$exclude_ref))
})

test_that("use_existing_checkpoint_results = TRUE reuses prior checkpoints", {
  d   <- make_mini()
  dir <- file.path(tempdir(), "gc_ckpt_resume")
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  cleangrowth_checkpoint(d, dir, chunk_size = 3, quietly = TRUE)  # establish checkpoints

  # tamper chunk_0001 with a recognizable value so reuse is detectable
  c1 <- readRDS(file.path(dir, "chunk_0001.rds"))
  c1$measurement[1] <- 999.0
  saveRDS(c1, file.path(dir, "chunk_0001.rds"))

  res_reuse <- cleangrowth_checkpoint(d, dir, chunk_size = 3,
                                      use_existing_checkpoint_results = TRUE,
                                      quietly = TRUE)
  expect_true(999.0 %in% res_reuse$measurement)   # tamper survived -> reused

  res_fresh <- cleangrowth_checkpoint(d, dir, chunk_size = 3, quietly = TRUE)
  expect_false(999.0 %in% res_fresh$measurement)  # default overwrote it
})

test_that("explicit chunk_size that exceeds the 1000-chunk cap errors", {
  big <- data.table::data.table(
    subjid = 1:1001, param = "WEIGHTKG", agedays = 365L,
    sex = 0L, measurement = 12.0
  )
  big[, id := .I]
  dir <- file.path(tempdir(), "gc_ckpt_cap")
  unlink(dir, recursive = TRUE)
  expect_error(
    cleangrowth_checkpoint(big, dir, chunk_size = 1, quietly = TRUE),
    "1000-chunk limit"
  )
})
