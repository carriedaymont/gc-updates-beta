# -- Internal helper -----------------------------------------------------------

#' Combine checkpoint .rds files into a single data.table
#'
#' Internal workhorse used by both \code{cleangrowth_checkpoint()} and
#' \code{cleangrowth_checkpoint_combine()}.
#'
#' @param ckpt_files Character vector of full paths to chunk .rds files,
#'   in the order they should be combined.
#' @param id_order Optional vector of id values in the desired output row
#'   order. If NULL, rows are returned in chunk order.
#' @param .log Function for logging messages.
#' @return data.table
#' @noRd
.combine_checkpoints <- function(ckpt_files, id_order = NULL, .log = message) {

  # Verify all expected files exist before reading any
  missing <- ckpt_files[!file.exists(ckpt_files)]
  if (length(missing) > 0)
    stop("Cannot combine: ", length(missing), " checkpoint file(s) missing:\n",
         paste(basename(missing), collapse = ", "))

  .log("Reading and combining ", length(ckpt_files), " checkpoint file(s) one at a time...")

  # Read and bind one file at a time to limit peak memory usage.
  # Loading all files into a list first would hold up to n+1 copies in RAM;
  # here at most 2 copies (final + current chunk) are live at once.
  first_levels <- NULL   # factor levels from first chunk that has them
  final        <- NULL

  for (i in seq_along(ckpt_files)) {
    chunk <- readRDS(ckpt_files[[i]])

    # Capture factor levels before rbindlist can coerce them
    if (is.null(first_levels) &&
        "exclude" %in% names(chunk) && is.factor(chunk[["exclude"]]))
      first_levels <- levels(chunk[["exclude"]])

    final <- if (is.null(final)) chunk
             else rbindlist(list(final, chunk), use.names = TRUE, fill = TRUE)
    rm(chunk)   # release per-chunk memory immediately

    if (i %% 10L == 0L || i == length(ckpt_files))
      .log("  Read ", i, " / ", length(ckpt_files),
           " file(s), ", nrow(final), " rows so far")
  }

  # Re-apply factor levels to exclude column in case rbindlist coerced it.
  if (!is.null(first_levels) && "exclude" %in% names(final))
    final[, exclude := factor(as.character(exclude), levels = first_levels)]

  # Reorder to match id_order if provided
  if (!is.null(id_order)) {
    # Use character comparison to be safe against integer/double/factor type
    # mismatches between the input data and what was saved in the .rds files.
    idx <- match(as.character(id_order), as.character(final[["id"]]))
    n_missing <- sum(is.na(idx))
    if (n_missing > 0)
      warning(n_missing, " id(s) from id_order not found in checkpoint files. ",
              "These rows will appear as NA in the output.")
    final <- final[idx]
  }

  .log("Combined result: ", nrow(final), " rows, ", ncol(final), " columns")
  return(final)
}


# -- Main checkpoint function ---------------------------------------------------

#' Run cleangrowth() with automatic checkpointing for large datasets
#'
#' Splits subjects into chunks, saves each chunk's result to a checkpoint file,
#' and skips already-completed chunks on restart. When a long run is
#' interrupted or fails, re-running the same call resumes from the last
#' completed chunk rather than starting over.
#'
#' @param data data.table (or object coercible to one) with columns
#'   \code{subjid}, \code{param}, \code{agedays}, \code{sex},
#'   \code{measurement}, and \code{id}. Column names must match exactly.
#' @param checkpoint_dir Directory for per-chunk checkpoint files (\code{.rds}).
#'   Created if it does not exist. Use a dedicated directory per dataset/run
#'   to avoid mixing checkpoints from different runs.
#' @param chunk_size Number of subjects per chunk. Default 5000. Aim for
#'   20--60 minutes of runtime per chunk. The number of chunks is capped at
#'   1000: if the default would exceed that, \code{chunk_size} is automatically
#'   increased to stay within the cap; an explicitly supplied \code{chunk_size}
#'   that would exceed it is an error. When resuming with
#'   \code{use_existing_checkpoint_results = TRUE}, use the same \code{chunk_size}
#'   (and dataset) as the original run.
#' @param log_file Optional path to a log file. Messages are always written
#'   to the console; if provided, they are also appended to this file so a
#'   colleague can monitor progress remotely.
#' @param keep_checkpoints Logical. Controls only what happens to the per-chunk
#'   checkpoint files \emph{after} a successful run. If \code{TRUE} (default),
#'   they are left in \code{checkpoint_dir}; if \code{FALSE}, they are deleted
#'   once the combined result is returned. Independent of
#'   \code{use_existing_checkpoint_results}, which governs the start of the run.
#' @param use_existing_checkpoint_results Logical. If \code{FALSE} (default),
#'   the dataset is cleaned fresh: any checkpoint files already in
#'   \code{checkpoint_dir} from a prior run are deleted at the start, so the
#'   combined result contains only chunks from this run. If \code{TRUE},
#'   existing checkpoint files are reused -- chunks that already have a
#'   checkpoint are skipped -- to resume an interrupted run or re-combine prior
#'   results. When \code{TRUE}, you are responsible for using the same dataset
#'   and \code{chunk_size} as the run that wrote those files.
#' @param quietly Logical (default FALSE). When FALSE, prints progress: a run
#'   header, a prominent banner per chunk (the most visible "where am I" marker),
#'   the per-chunk cleaning milestones, and per-chunk plus total elapsed time.
#'   When TRUE, the console is silent; if \code{log_file} is set it still
#'   receives the full progress log for remote monitoring.
#' @param verbose Logical (default FALSE). When TRUE, additionally prints the
#'   granular per-step diagnostics from \code{cleangrowth()}. Implies
#'   \code{quietly = FALSE}.
#' @param ... Additional arguments passed to \code{\link{cleangrowth}} (e.g.
#'   \code{cf_rescue}, \code{adult_permissiveness}).
#'   \code{ref_tables} is handled internally - reference tables are pre-loaded
#'   once and reused across all chunks. Do not pass \code{ref_tables} here.
#'   \code{recenter_source} is forwarded to each chunk, so a chunked run
#'   recenters exactly like an unchunked one.
#'
#' @return A data.table with the same structure as \code{\link{cleangrowth}}
#'   output, with rows in the original input order.
#'
#' @details
#' Checkpoint files are named \code{chunk_0001.rds}, \code{chunk_0002.rds},
#' etc. in \code{checkpoint_dir}. By default a run starts fresh, deleting any
#' checkpoint files already present in \code{checkpoint_dir}. With
#' \code{use_existing_checkpoint_results = TRUE}, any chunk that already has a
#' checkpoint file is skipped, so an interrupted run resumes where it left off.
#'
#' If the run completes but the combine step fails, use
#' \code{\link{cleangrowth_checkpoint_combine}} to re-combine the saved
#' checkpoint files without re-running the algorithm.
#'
#' To check progress during a run, count the \code{.rds} files in
#' \code{checkpoint_dir} or read the log file. The log file is safe to read
#' while the run is in progress.
#'
#' \code{recenter_source} (\code{"reference"} or \code{"none"}) and any
#' user-supplied \code{sd.recenter} table passed via \code{...} are forwarded
#' to every chunk unchanged, so a chunked run recenters identically to an
#' unchunked one.
#'
#' For a run redirected to a log file from the shell:
#' \preformatted{Rscript run_gc.R >> results/run_log.txt 2>&1 &}
#'
#' @seealso \code{\link{cleangrowth_checkpoint_combine}}
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- fread("large_dataset.csv")
#'
#' result <- cleangrowth_checkpoint(
#'   data           = dt,
#'   checkpoint_dir = "results/gc_checkpoints",
#'   chunk_size     = 5000,
#'   log_file       = "results/gc_run.log"
#' )
#' }
#'
#' @export
cleangrowth_checkpoint <- function(data,
                                   checkpoint_dir,
                                   chunk_size       = 5000,
                                   log_file         = NULL,
                                   keep_checkpoints = TRUE,
                                   use_existing_checkpoint_results = FALSE,
                                   quietly          = FALSE,
                                   verbose          = FALSE,
                                   ...) {

  if (isTRUE(verbose)) quietly <- FALSE

  # Whether chunk_size was supplied by the caller (vs. left at the default).
  # Used by the chunk-count cap below: a default chunk_size auto-increases to
  # stay within the limit, but an explicit one that exceeds it errors.
  chunk_size_provided <- !missing(chunk_size)

  # ---- input validation ----

  if (!is.data.table(data)) data <- as.data.table(data)

  required_cols <- c("subjid", "param", "agedays", "sex", "measurement")
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0)
    stop("data is missing required columns: ",
         paste(missing_cols, collapse = ", "))

  # `id` is fully optional. If the column is missing / has NA / has duplicates,
  # warn and auto-generate sequential ids (1:N over the whole dataset, in input
  # row order). user_id_orig snapshots the user's original `id` column (or NULL
  # if absent) so we can restore it to the final output `id` column after the
  # combine; the auto-generated id gc actually used internally is surfaced as
  # `gc_id` when `display_gc_id = TRUE` (or `full_detail = TRUE`) is passed via
  # `...`. Whole-dataset regen here keeps the per-chunk ids consistent (each
  # chunk receives an already-valid id, so cleangrowth() does not re-regen).
  user_id_orig <- if ("id" %in% names(data)) data[["id"]] else NULL
  if (!("id" %in% names(data))) {
    warning("`id` column not provided; auto-generating sequential ids 1:N. ",
            "Provide an `id` column for stable row identifiers.")
    data[, id := seq_len(.N)]
  } else if (anyNA(data$id)) {
    warning("`id` contains missing (NA) value(s); auto-generating sequential ids 1:N in input row order. ",
            "Your original `id` is preserved in the output `id` column; the auto-generated id gc used internally is available as `gc_id` (set `display_gc_id = TRUE` or `full_detail = TRUE`). ",
            "Supply a complete, unique `id` for stable row identifiers.")
    data[, id := seq_len(.N)]
  } else if (anyDuplicated(data$id)) {
    dup_ids <- unique(data$id[duplicated(data$id)])
    ex <- dup_ids[seq_len(min(5L, length(dup_ids)))]
    warning(sprintf(
      paste0("`id` contains %d duplicated value(s) (%s%s); auto-generating sequential ids 1:N in input row order. ",
             "A common cause is passing a per-subject or per-encounter identifier as `id` instead of a unique per-row id. ",
             "Your original `id` is preserved in the output `id` column; the auto-generated id gc used internally is available as `gc_id` (set `display_gc_id = TRUE` or `full_detail = TRUE`). ",
             "For stable row identifiers, supply a unique per-row `id` (e.g. `data[, id := .I]`)."),
      length(dup_ids),
      paste(ex, collapse = ", "),
      if (length(dup_ids) > 5L) ", ..." else ""
    ))
    data[, id := seq_len(.N)]
  }

  if (!is.numeric(chunk_size) || length(chunk_size) != 1L || chunk_size <= 0)
    stop("chunk_size must be a single positive number")

  if (!is.null(log_file) && !is.character(log_file))
    stop("log_file must be a character string path or NULL")

  # ---- setup ----

  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

  t_run_start <- Sys.time()

  # .log(): timestamped status line. Console only when !quietly; always appended
  # to log_file (if set) so a colleague can monitor a quiet run remotely.
  .log <- function(...) {
    msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)
    if (!quietly) message(msg)
    if (!is.null(log_file))
      cat(msg, "\n", file = log_file, append = TRUE)
  }

  # .banner(): prominent, un-timestamped chunk marker - the line the user scans
  # for to see how far the run has progressed. Tee'd to log_file as well.
  .banner <- function(title) {
    rule <- strrep("=", 50L)
    lines <- c("", rule, paste0("  ", title), rule)
    if (!quietly) for (ln in lines) message(ln)
    if (!is.null(log_file))
      cat(paste(lines, collapse = "\n"), "\n", file = log_file, append = TRUE)
  }

  # Compact elapsed-time formatter ("32s" / "18m04s" / "1h02m").
  .fmt_dur <- function(secs) {
    secs <- as.numeric(secs)
    if (secs < 60) return(sprintf("%.0fs", secs))
    if (secs < 3600) return(sprintf("%dm%02ds", secs %/% 60, round(secs %% 60)))
    sprintf("%dh%02dm", secs %/% 3600, (secs %% 3600) %/% 60)
  }

  # ---- split subjects into chunks ----

  # Sort so chunk membership is deterministic for a given set of subjects and
  # chunk_size, independent of input row order. The final result is reordered
  # to the input id order regardless, so output order is unaffected.
  all_subjs <- sort(unique(data$subjid))
  n_subjs   <- length(all_subjs)

  # Cap the number of chunks. A default chunk_size auto-increases to stay within
  # the limit; an explicitly supplied chunk_size that would exceed it errors.
  max_chunks <- 1000L
  if (ceiling(n_subjs / chunk_size) > max_chunks) {
    min_chunk_size <- ceiling(n_subjs / max_chunks)
    if (chunk_size_provided) {
      stop("chunk_size = ", chunk_size, " would produce ",
           ceiling(n_subjs / chunk_size), " chunks for ", n_subjs,
           " subjects, exceeding the ", max_chunks, "-chunk limit. ",
           "Increase chunk_size to at least ", min_chunk_size, ".")
    }
    chunk_size <- min_chunk_size
    .log("Auto-increased chunk_size to ", chunk_size,
         " to stay within ", max_chunks, " chunks for ", n_subjs, " subjects.")
  }

  chunks    <- split(all_subjs,
                     ceiling(seq_along(all_subjs) / chunk_size))
  n_chunks  <- length(chunks)

  .log("growthcleanr \u2014 ", format(n_subjs, big.mark = ","), " subjects, ",
       format(nrow(data), big.mark = ","), " measurements, ",
       n_chunks, " chunk", if (n_chunks == 1L) "" else "s",
       " of ", format(chunk_size, big.mark = ","),
       " | checkpoint_dir: ", normalizePath(checkpoint_dir, mustWork = FALSE))

  # ---- pre-load reference tables once across all chunks ----
  # Saves ~0.9 sec per cleangrowth() call.

  extra_args <- list(...)
  if ("ref_tables" %in% names(extra_args)) {
    warning("ref_tables passed via ... will be ignored; ",
            "cleangrowth_checkpoint() pre-loads reference tables internally.")
    extra_args$ref_tables <- NULL
  }
  extra_args$ref_tables <- gc_preload_refs()

  # ---- reset or reuse prior checkpoint files ----
  # By default each run cleans the dataset fresh: any checkpoint files left in
  # checkpoint_dir from a prior run are removed, so the combined result contains
  # only this run's chunks. Set use_existing_checkpoint_results = TRUE to reuse
  # them instead (resume an interrupted run, or re-combine prior results).

  existing_ckpts <- list.files(checkpoint_dir,
                               pattern = "^chunk_\\d{4}\\.rds$",
                               full.names = TRUE)
  if (length(existing_ckpts) > 0) {
    if (use_existing_checkpoint_results) {
      .log("use_existing_checkpoint_results = TRUE: reusing ",
           length(existing_ckpts),
           " existing checkpoint file(s); chunks that already have one are skipped.")
    } else {
      file.remove(existing_ckpts)
      .log("Removed ", length(existing_ckpts),
           " checkpoint file(s) from a prior run (use_existing_checkpoint_results = FALSE).")
    }
  }

  # ---- recenter_source validation ----
  # recenter_source is forwarded to each chunk's cleangrowth() call (via ...),
  # so a chunked run recenters exactly like an unchunked one. "none" builds an
  # all-zero table inside each chunk; a user-supplied sd.recenter (via ...) is
  # likewise forwarded to every chunk unchanged.
  recenter_source <- extra_args$recenter_source
  if (is.null(recenter_source)) recenter_source <- "reference"
  if (!(length(recenter_source) == 1L && recenter_source %in% c("reference", "none")))
    stop('recenter_source must be "reference" or "none".')

  # ---- process chunks ----

  for (i in seq_along(chunks)) {

    ckpt_file <- file.path(checkpoint_dir, sprintf("chunk_%04d.rds", i))

    if (use_existing_checkpoint_results && file.exists(ckpt_file)) {
      .log("chunk ", i, "/", n_chunks, " \u2014 skipped (already complete)")
      next
    }

    chunk_data <- data[subjid %in% chunks[[i]]]
    .banner(sprintf("CHUNK %d / %d   \u00b7   %s subjects",
                    i, n_chunks, format(length(chunks[[i]]), big.mark = ",")))
    t_chunk_start <- Sys.time()

    # cleangrowth() prints its own milestone lines (preprocessing / cleaning
    # child / cleaning adult). .checkpoint_driven = TRUE tells it to skip its own
    # header / banner / total - the per-chunk framing is owned here.
    # Use [[ ]] access so cleangrowth() output uses canonical column names
    # (subjid, param, etc.) regardless of the calling context - needed for
    # consistent combination across chunks.
    result <- do.call(cleangrowth, c(
      list(
        subjid      = chunk_data[["subjid"]],
        param       = chunk_data[["param"]],
        agedays     = chunk_data[["agedays"]],
        sex         = chunk_data[["sex"]],
        measurement = chunk_data[["measurement"]],
        id          = chunk_data[["id"]],
        quietly           = quietly,
        verbose           = verbose,
        .checkpoint_driven = TRUE
      ),
      extra_args
    ))

    saveRDS(result, ckpt_file)
    .log("chunk ", i, "/", n_chunks, " \u2014 done \u00b7 ",
         .fmt_dur(difftime(Sys.time(), t_chunk_start, units = "secs")),
         " \u00b7 elapsed ",
         .fmt_dur(difftime(Sys.time(), t_run_start, units = "secs")))
  }

  # ---- combine all chunks in original input order ----

  ckpt_files <- file.path(checkpoint_dir,
                           sprintf("chunk_%04d.rds", seq_len(n_chunks)))

  .log("postprocessing \u2014 combining ", n_chunks, " chunk",
       if (n_chunks == 1L) "" else "s")
  # Per-file read progress is verbose-only; the milestone + total lines above and
  # below cover the standard view.
  final <- .combine_checkpoints(ckpt_files,
                                 id_order = data[["id"]],
                                 .log = if (verbose) .log else function(...) invisible(NULL))

  # Sanity check: combined row count should match input
  if (nrow(final) != nrow(data))
    warning("Combined result has ", nrow(final), " rows but input had ",
            nrow(data), " rows. Check checkpoint files for corruption.")

  if (!keep_checkpoints) {
    file.remove(ckpt_files)
    .log("Checkpoint files removed")
  }

  # Restore the user's original `id` to the output `id` column. After the
  # combine, `final` rows are ordered by id_order = data[["id"]], which equals
  # input row order (id_order is the auto-regen 1:N in row order when we had
  # to regen; otherwise it's the user's valid id and the restore is a no-op).
  # user_id_orig (snapshotted before regen) is also in input row order, so
  # positional replace aligns. If the user never supplied an `id` column,
  # user_id_orig is NULL and the regen stays.
  if (!is.null(user_id_orig)) {
    if (length(user_id_orig) == nrow(final)) {
      final[, id := user_id_orig]
    } else {
      warning("Could not restore user-supplied `id` to output: length mismatch ",
              "(input had ", length(user_id_orig), " rows, output has ",
              nrow(final), "). The auto-generated id is retained in `id`; ",
              "gc_id (if set) reflects the id gc used internally.")
    }
  }

  .log("all ", n_chunks, " chunk", if (n_chunks == 1L) "" else "s",
       " complete \u00b7 ", format(n_subjs, big.mark = ","), " subjects, ",
       format(nrow(final), big.mark = ","), " rows \u00b7 total ",
       .fmt_dur(difftime(Sys.time(), t_run_start, units = "secs")))
  return(final)
}


# -- Standalone combine function ------------------------------------------------

#' Combine saved checkpoint files from a cleangrowth_checkpoint() run
#'
#' Reads all checkpoint \code{.rds} files from a directory and combines them
#' into a single data.table. Use this when \code{\link{cleangrowth_checkpoint}}
#' completed all chunks but the combine step failed - you can re-combine
#' without re-running the algorithm.
#'
#' @param checkpoint_dir Directory containing checkpoint \code{.rds} files
#'   named \code{chunk_0001.rds}, \code{chunk_0002.rds}, etc.
#' @param id_order Optional vector of \code{id} values specifying the desired
#'   output row order (typically \code{your_data$id}). If \code{NULL}, rows
#'   are returned in chunk order (subjects grouped together but not in the
#'   original row order).
#' @param log_file Optional path to append log messages.
#' @param quietly Logical (default FALSE). When FALSE, prints a short start and
#'   completion line (and writes them to \code{log_file} if set). When TRUE, the
#'   console is silent; \code{log_file} still receives the log.
#' @param verbose Logical (default FALSE). When TRUE, additionally prints the
#'   per-file read progress. Implies \code{quietly = FALSE}.
#'
#' @return A data.table with the same structure as \code{\link{cleangrowth}}
#'   output.
#'
#' @details
#' All \code{.rds} files matching \code{chunk_NNNN.rds} in
#' \code{checkpoint_dir} are read and combined in numeric order. Every matching
#' file is included, so the directory should hold checkpoints from a single run
#' only. (\code{cleangrowth_checkpoint()} clears the directory at the start of a
#' fresh run, so after one of its runs only that run's files remain.)
#'
#' @seealso \code{\link{cleangrowth_checkpoint}}
#'
#' @examples
#' \dontrun{
#' # If the combine step failed at the end of a run, re-combine like this:
#' result <- cleangrowth_checkpoint_combine(
#'   checkpoint_dir = "results/gc_checkpoints",
#'   id_order       = dt$id   # for original row order
#' )
#' saveRDS(result, "results/gc_output.rds")
#' }
#'
#' @export
cleangrowth_checkpoint_combine <- function(checkpoint_dir,
                                            id_order = NULL,
                                            log_file = NULL,
                                            quietly  = FALSE,
                                            verbose  = FALSE) {

  if (isTRUE(verbose)) quietly <- FALSE

  if (!dir.exists(checkpoint_dir))
    stop("checkpoint_dir does not exist: ", checkpoint_dir)

  t_start <- Sys.time()
  .fmt_dur <- function(secs) {
    secs <- as.numeric(secs)
    if (secs < 60) return(sprintf("%.0fs", secs))
    if (secs < 3600) return(sprintf("%dm%02ds", secs %/% 60, round(secs %% 60)))
    sprintf("%dh%02dm", secs %/% 3600, (secs %% 3600) %/% 60)
  }
  .log <- function(...) {
    msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)
    if (!quietly) message(msg)
    if (!is.null(log_file))
      cat(msg, "\n", file = log_file, append = TRUE)
  }

  # Auto-detect chunk files in numeric order
  all_files <- list.files(checkpoint_dir,
                           pattern = "^chunk_\\d{4}\\.rds$",
                           full.names = TRUE)
  if (length(all_files) == 0)
    stop("No chunk_NNNN.rds files found in: ", checkpoint_dir)

  ckpt_files <- sort(all_files)   # lexicographic == numeric for zero-padded names
  .log("combining ", length(ckpt_files),
       " checkpoint file(s) in ", normalizePath(checkpoint_dir))

  # Per-file read progress is verbose-only.
  final <- .combine_checkpoints(ckpt_files, id_order = id_order,
                                .log = if (verbose) .log else function(...) invisible(NULL))

  .log("done \u00b7 ", format(nrow(final), big.mark = ","), " rows \u00b7 ",
       .fmt_dur(difftime(Sys.time(), t_start, units = "secs")))
  return(final)
}
