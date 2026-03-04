# tests/testthat/test-history.R

# ---------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------

Q_hist <- matrix(c(
  1, 0,
  1, 0,
  0, 1,
  0, 1,
  1, 1,
  1, 1
), nrow = 6, ncol = 2, byrow = TRUE)

items_hist <- cdcat_items(
  Q_hist, "DINA",
  slip  = rep(0.10, 6),
  guess = rep(0.20, 6)
)

content_hist <- c("A", "A", "B", "B", "C", "C")
content_prop_hist <- c(A = 1/3, B = 1/3, C = 1/3)
exposure_sh_hist  <- rep(0.9, 6)     # Sympson-Hetter
exposure_rq_hist  <- rep(2L, 6)      # Randomesque

# Run a full 6-item session and return the session object
run_full <- function(...) {
  s <- CdcatSession$new(items_hist, min_items = 6L, max_items = 6L, ...)
  resp <- c(1, 0, 1, 0, 1, 0)
  repeat {
    item <- s$next_item()
    if (item == 0) break
    s$update(item, resp[item])
  }
  s
}


# ---------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------

test_that(".compute_mastery_marginal() returns named vector of length K", {
  est <- estimate_alpha(rep(NA, 6), items_hist)
  mm  <- .compute_mastery_marginal(est$posterior, est$skill_patterns)
  expect_length(mm, 2L)
  expect_named(mm, c("A1", "A2"))
  expect_true(all(mm >= 0 & mm <= 1))
})

test_that(".compute_mastery_marginal() values sum with complement to 1 per attribute", {
  est <- estimate_alpha(c(1, 1, NA, NA, NA, NA), items_hist)
  mm  <- .compute_mastery_marginal(est$posterior, est$skill_patterns)
  # Each marginal in [0,1] and consistent with posterior
  sp  <- est$skill_patterns
  for (k in seq_len(ncol(sp))) {
    expected <- sum(est$posterior[sp[, k] == 1])
    expect_equal(mm[[k]], expected, tolerance = 1e-8)
  }
})

test_that(".compute_domain_gap() returns NULL when content is NULL", {
  expect_null(.compute_domain_gap(integer(0), NULL, NULL))
})

test_that(".compute_domain_gap() returns full gap when no items administered", {
  gap <- .compute_domain_gap(integer(0), content_hist, content_prop_hist)
  expect_named(gap, c("A", "B", "C"))
  expect_equal(as.numeric(gap), c(1/3, 1/3, 1/3), tolerance = 1e-8)
})

test_that(".compute_domain_gap() updates after administering items", {
  # After 2 A items administered, A gap should be negative
  gap <- .compute_domain_gap(c(1L, 2L), content_hist, content_prop_hist)
  expect_true(gap["A"] < 0)
  expect_true(gap["B"] > 0)
  expect_true(gap["C"] > 0)
})

test_that(".build_history_step() produces correct structure", {
  est <- estimate_alpha(c(1, NA, NA, NA, NA, NA), items_hist)
  pending <- list(
    candidate_items     = 2:6,
    criterion_scores    = setNames(runif(5), 2:6),
    item_pre_exposure   = 3L,
    exposure_redirected = FALSE,
    target_domain       = "B",
    domain_gap          = c(A = -0.1, B = 0.2, C = 0.1),
    stop_check          = list(stop = FALSE, reason = "continuing")
  )
  now <- Sys.time()
  rec <- .build_history_step(1L, 1L, 1L, pending, est, now, 2.5)

  expect_equal(rec$step, 1L)
  expect_equal(rec$item, 1L)
  expect_equal(rec$response, 1L)
  expect_equal(rec$response_time, 2.5)
  expect_equal(rec$target_domain, "B")
  expect_length(rec$posterior, 4L)
  expect_length(rec$mastery_marginal, 2L)
  expect_equal(sum(rec$posterior), 1, tolerance = 1e-6)
})

test_that(".history_to_df() returns empty data.frame for empty history", {
  df <- .history_to_df(list())
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
})

test_that(".history_to_df() produces correct number of rows", {
  s  <- run_full()
  df <- .history_to_df(s$history)
  expect_equal(nrow(df), 6L)
})

test_that(".history_to_df() contains expected scalar columns", {
  s   <- run_full()
  df  <- .history_to_df(s$history)
  expected_cols <- c("step", "item", "response",
                     "item_pre_exposure", "exposure_redirected",
                     "target_domain", "alpha_hat_index",
                     "timestamp", "response_time",
                     "stop_check_stop", "stop_check_reason")
  expect_true(all(expected_cols %in% names(df)))
})

test_that(".history_to_df() contains list-columns for vector fields", {
  s  <- run_full()
  df <- .history_to_df(s$history)
  expect_true(is.list(df$posterior))
  expect_true(is.list(df$mastery_marginal))
  expect_true(is.list(df$alpha_hat))
  expect_true(is.list(df$criterion_scores))
  expect_true(is.list(df$candidate_items))
  expect_true(is.list(df$domain_gap))
})


# ---------------------------------------------------------------
# select_next_item() return_details
# ---------------------------------------------------------------

test_that("select_next_item() return_details=FALSE returns integer", {
  item <- select_next_item(items_hist, rep(NA, 6), integer(0), "PWKL")
  expect_type(item, "integer")
})

test_that("select_next_item() return_details=TRUE returns named list", {
  sel <- select_next_item(items_hist, rep(NA, 6), integer(0), "PWKL",
                          return_details = TRUE)
  expect_named(sel, c("item", "candidate_items", "criterion_scores",
                      "item_pre_exposure", "exposure_redirected"))
})

test_that("select_next_item() details: item is in candidate_items", {
  sel <- select_next_item(items_hist, rep(NA, 6), integer(0), "PWKL",
                          return_details = TRUE)
  expect_true(sel$item %in% sel$candidate_items)
})

test_that("select_next_item() details: criterion_scores covers all candidates", {
  sel <- select_next_item(items_hist, rep(NA, 6), integer(0), "PWKL",
                          return_details = TRUE)
  expect_equal(length(sel$criterion_scores), length(sel$candidate_items))
  expect_named(sel$criterion_scores)
})

test_that("select_next_item() details: item_pre_exposure matches greedy best when no exposure", {
  sel <- select_next_item(items_hist, rep(NA, 6), integer(0), "KL",
                          return_details = TRUE)
  # Without exposure, item == item_pre_exposure
  expect_equal(sel$item, sel$item_pre_exposure)
  expect_false(sel$exposure_redirected)
})

test_that("select_next_item() details: criterion_scores is NULL for SEQ", {
  sel <- select_next_item(items_hist, rep(NA, 6), integer(0), "SEQ",
                          return_details = TRUE)
  expect_null(sel$criterion_scores)
})

test_that("select_next_item() details: criterion_scores is NULL for RANDOM", {
  sel <- select_next_item(items_hist, rep(NA, 6), integer(0), "RANDOM",
                          return_details = TRUE)
  expect_null(sel$criterion_scores)
})

test_that("select_next_item() details: shadow mode returns full-bank scores", {
  gs  <- function(scores, items, administered) {
    scores[administered] <- -Inf; which.max(scores)
  }
  sel <- select_next_item(items_hist, rep(NA, 6), integer(0), "PWKL",
                          constr_fun = gs, return_details = TRUE)
  expect_equal(length(sel$criterion_scores), items_hist$n_items)
})


# ---------------------------------------------------------------
# CdcatSession history -- structure
# ---------------------------------------------------------------

test_that("history is empty at session start", {
  s <- CdcatSession$new(items_hist)
  expect_length(s$history, 0L)
})

test_that("history grows by one entry per update()", {
  s <- CdcatSession$new(items_hist, min_items = 3L, max_items = 3L)
  for (i in seq_len(3)) {
    item <- s$next_item()
    s$update(item, 1L)
    expect_length(s$history, i)
  }
})

test_that("each history entry has all required fields", {
  s   <- run_full()
  rec <- s$history[[1]]
  required <- c("step", "item", "response",
                "candidate_items", "criterion_scores",
                "item_pre_exposure", "exposure_redirected",
                "target_domain", "domain_gap",
                "stop_check",
                "posterior", "alpha_hat", "alpha_hat_index",
                "mastery_marginal", "timestamp", "response_time")
  expect_true(all(required %in% names(rec)))
})

test_that("history step numbers are sequential", {
  s <- run_full()
  steps <- sapply(s$history, function(h) h$step)
  expect_equal(steps, seq_len(6))
})

test_that("history items match administered order", {
  s <- run_full()
  hist_items <- sapply(s$history, function(h) h$item)
  expect_equal(hist_items, s$administered)
})

test_that("history responses match recorded responses", {
  s    <- run_full()
  resp <- c(1, 0, 1, 0, 1, 0)
  for (rec in s$history) {
    expect_equal(rec$response, resp[rec$item])
  }
})

test_that("history posteriors are valid distributions", {
  s <- run_full()
  for (rec in s$history) {
    expect_equal(sum(rec$posterior), 1, tolerance = 1e-6)
    expect_true(all(rec$posterior >= 0))
  }
})

test_that("history mastery_marginal values are in [0,1]", {
  s <- run_full()
  for (rec in s$history) {
    expect_true(all(rec$mastery_marginal >= 0 & rec$mastery_marginal <= 1))
  }
})

test_that("history alpha_hat is a binary vector of length K", {
  s <- run_full()
  for (rec in s$history) {
    expect_length(rec$alpha_hat, 2L)
    expect_true(all(rec$alpha_hat %in% c(0L, 1L)))
  }
})

test_that("history timestamps are non-decreasing", {
  s  <- run_full()
  ts <- sapply(s$history, function(h) as.numeric(h$timestamp))
  expect_true(all(diff(ts) >= 0))
})

test_that("history response_time values are non-negative", {
  s <- run_full()
  rt <- sapply(s$history, function(h) h$response_time)
  expect_true(all(rt >= 0))
})

test_that("history stop_check is FALSE for all mid-session steps", {
  s <- run_full()
  for (rec in s$history) {
    expect_false(rec$stop_check$stop)
  }
})

test_that("history alpha_hat_index matches alpha_hat row in skill_patterns", {
  s <- run_full()
  sp <- s$est$skill_patterns
  for (rec in s$history) {
    profile_from_index <- as.integer(sp[rec$alpha_hat_index, ])
    expect_equal(profile_from_index, rec$alpha_hat)
  }
})


# ---------------------------------------------------------------
# CdcatSession history -- content balancing fields
# ---------------------------------------------------------------

test_that("history target_domain is NULL when no content balancing", {
  s <- run_full()
  for (rec in s$history) expect_null(rec$target_domain)
})

test_that("history target_domain is non-NULL with content balancing", {
  s <- run_full(content = content_hist, content_prop = content_prop_hist)
  for (rec in s$history) expect_false(is.null(rec$target_domain))
})

test_that("history domain_gap is NULL when no content balancing", {
  s <- run_full()
  for (rec in s$history) expect_null(rec$domain_gap)
})

test_that("history domain_gap has correct names with content balancing", {
  s <- run_full(content = content_hist, content_prop = content_prop_hist)
  for (rec in s$history) {
    expect_named(rec$domain_gap, c("A", "B", "C"))
  }
})

test_that("history domain_gap sums to zero after first step (proportions sum to 1)", {
  # gap = target - observed; sum(target) = 1, sum(observed) = 1 after first step
  s <- run_full(content = content_hist, content_prop = content_prop_hist)
  # From step 2 onwards (one item administered -> observed sums to 1)
  for (rec in s$history[-1]) {
    expect_equal(sum(rec$domain_gap), 0, tolerance = 1e-8)
  }
})


# ---------------------------------------------------------------
# CdcatSession history -- exposure fields
# ---------------------------------------------------------------

test_that("history exposure_redirected is always FALSE without exposure control", {
  s <- run_full()
  for (rec in s$history) expect_false(rec$exposure_redirected)
})

test_that("history item_pre_exposure is NA for SEQ criterion", {
  s <- CdcatSession$new(items_hist, criterion = "SEQ",
                        min_items = 6L, max_items = 6L)
  resp <- c(1, 0, 1, 0, 1, 0)
  repeat {
    item <- s$next_item(); if (item == 0) break; s$update(item, resp[item])
  }
  for (rec in s$history) expect_true(is.na(rec$item_pre_exposure))
})

test_that("history item_pre_exposure equals item when no exposure redirects", {
  s <- run_full()
  for (rec in s$history) {
    expect_equal(rec$item_pre_exposure, rec$item)
  }
})


# ---------------------------------------------------------------
# CdcatSession history -- shadow CAT fields
# ---------------------------------------------------------------

test_that("history criterion_scores covers full bank in shadow mode", {
  gs <- function(scores, items, administered) {
    scores[administered] <- -Inf; which.max(scores)
  }
  s <- CdcatSession$new(items_hist, constr_fun = gs,
                        min_items = 4L, max_items = 4L)
  resp <- c(1, 0, 1, 0, 1, 0)
  repeat {
    item <- s$next_item(); if (item == 0) break; s$update(item, resp[item])
  }
  for (rec in s$history) {
    expect_equal(length(rec$criterion_scores), items_hist$n_items)
  }
})


# ---------------------------------------------------------------
# CdcatSession history_df()
# ---------------------------------------------------------------

test_that("history_df() returns empty data.frame before any update", {
  s  <- CdcatSession$new(items_hist)
  df <- s$history_df()
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
})

test_that("history_df() has one row per administered item", {
  s  <- run_full()
  df <- s$history_df()
  expect_equal(nrow(df), 6L)
})

test_that("history_df() step column is 1:n", {
  s  <- run_full()
  df <- s$history_df()
  expect_equal(df$step, 1:6)
})

test_that("history_df() item column matches administered", {
  s  <- run_full()
  df <- s$history_df()
  expect_equal(df$item, s$administered)
})

test_that("history_df() posterior list-column has correct lengths", {
  s  <- run_full()
  df <- s$history_df()
  # K=2 -> 2^2=4 profiles
  for (p in df$posterior) expect_length(p, 4L)
})

test_that("history_df() mastery_marginal list-column has length K", {
  s  <- run_full()
  df <- s$history_df()
  for (mm in df$mastery_marginal) expect_length(mm, 2L)
})

test_that("history_df() response_time column is numeric and non-negative", {
  s  <- run_full()
  df <- s$history_df()
  expect_true(is.numeric(df$response_time))
  expect_true(all(df$response_time >= 0))
})

test_that("history_df() works with content balancing active", {
  s  <- run_full(content = content_hist, content_prop = content_prop_hist)
  df <- s$history_df()
  expect_equal(nrow(df), 6L)
  expect_true(is.list(df$domain_gap))
  expect_false(any(sapply(df$domain_gap, is.null)))
})

test_that("history_df() timestamp column is POSIXct", {
  s  <- run_full()
  df <- s$history_df()
  expect_s3_class(df$timestamp, "POSIXct")
})


# ---------------------------------------------------------------
# Manual update() without next_item()
# ---------------------------------------------------------------

test_that("update() without next_item() still appends history", {
  s <- CdcatSession$new(items_hist)
  s$update(1L, 1L)
  expect_length(s$history, 1L)
  expect_equal(s$history[[1]]$item, 1L)
})

test_that("update() without next_item() records 'manual' stop_check reason", {
  s <- CdcatSession$new(items_hist)
  s$update(1L, 1L)
  expect_equal(s$history[[1]]$stop_check$reason, "manual")
})

test_that("update() without next_item() records empty candidate_items", {
  s <- CdcatSession$new(items_hist)
  s$update(1L, 0L)
  expect_length(s$history[[1]]$candidate_items, 0L)
})


# ---------------------------------------------------------------
# Print shows history count
# ---------------------------------------------------------------

test_that("print() shows history steps count", {
  s   <- run_full()
  out <- capture.output(s$print())
  expect_true(any(grepl("History.*6", out)))
})
