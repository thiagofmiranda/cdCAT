# tests/testthat/test-prior.R

Q     <- matrix(c(1,0, 0,1, 1,1), nrow = 3, ncol = 2, byrow = TRUE)
items <- cdcat_items(Q, "DINA", slip = c(0.1,0.1,0.1), guess = c(0.2,0.2,0.2))
# K=2 → 4 profiles in expand.grid order: "00","10","01","11"

# ---------------------------------------------------------------
# cdcat_prior() — constructor
# ---------------------------------------------------------------

test_that("cdcat_prior() returns named numeric vector summing to 1", {
  p <- cdcat_prior("00" = 0.4, "10" = 0.2, "01" = 0.2, "11" = 0.2)
  expect_true(is.numeric(p))
  expect_length(p, 4)
  expect_equal(sum(p), 1, tolerance = 1e-8)
  expect_equal(names(p), c("00", "10", "01", "11"))
})

test_that("cdcat_prior() reorders patterns to canonical order", {
  # Passed in non-canonical order; result must be reordered
  p <- cdcat_prior("11" = 0.4, "00" = 0.3, "01" = 0.2, "10" = 0.1)
  expect_equal(p[["00"]], 0.3)
  expect_equal(p[["10"]], 0.1)
  expect_equal(p[["01"]], 0.2)
  expect_equal(p[["11"]], 0.4)
})

test_that("cdcat_prior() auto-normalizes with warning", {
  expect_warning(
    p <- cdcat_prior("00" = 2, "10" = 1, "01" = 1, "11" = 0),
    "Normalizing"
  )
  expect_equal(sum(p), 1, tolerance = 1e-8)
})

test_that("cdcat_prior() errors on missing patterns", {
  expect_error(
    cdcat_prior("00" = 0.5, "10" = 0.5),
    "Missing prior probabilities"
  )
})

test_that("cdcat_prior() errors on invalid pattern name", {
  expect_error(
    cdcat_prior("00" = 0.25, "10" = 0.25, "01" = 0.25, "XX" = 0.25),
    "Invalid pattern names"
  )
})

test_that("cdcat_prior() errors on negative values", {
  expect_error(
    cdcat_prior("00" = -0.1, "10" = 0.4, "01" = 0.4, "11" = 0.3),
    "non-negative"
  )
})

test_that("cdcat_prior() errors when all values are zero", {
  expect_error(
    cdcat_prior("00" = 0, "10" = 0, "01" = 0, "11" = 0),
    "at least one positive"
  )
})

test_that("cdcat_prior() errors on inconsistent pattern lengths", {
  expect_error(
    cdcat_prior("0" = 0.5, "10" = 0.5),
    "same length"
  )
})

# ---------------------------------------------------------------
# .validate_prior() — internal validator
# ---------------------------------------------------------------

test_that(".validate_prior() returns NULL for NULL input", {
  expect_null(.validate_prior(NULL, K = 2))
})

test_that(".validate_prior() accepts valid vector", {
  p <- c(0.4, 0.2, 0.2, 0.2)
  result <- .validate_prior(p, K = 2)
  expect_equal(result, p, tolerance = 1e-8)
})

test_that(".validate_prior() errors on wrong length", {
  expect_error(.validate_prior(c(0.5, 0.5), K = 2), "length 2\\^K")
})

test_that(".validate_prior() errors on NA", {
  expect_error(.validate_prior(c(0.25, NA, 0.25, 0.5), K = 2), "NA")
})

test_that(".validate_prior() errors on negative value", {
  expect_error(.validate_prior(c(-0.1, 0.4, 0.4, 0.3), K = 2), "non-negative")
})

test_that(".validate_prior() auto-normalizes with warning", {
  expect_warning(
    result <- .validate_prior(c(2, 1, 1, 0), K = 2),
    "Normalizing"
  )
  expect_equal(sum(result), 1, tolerance = 1e-8)
})

# ---------------------------------------------------------------
# estimate_alpha() — with informative prior
# ---------------------------------------------------------------

test_that("informative prior concentrating on [1,1] shifts posterior before any response", {
  # Prior heavily favors profile [1,1] (index 4 in expand.grid order)
  p <- cdcat_prior("00" = 0.01, "10" = 0.01, "01" = 0.01, "11" = 0.97)
  est <- estimate_alpha(c(NA, NA, NA), items, prior = p)
  expect_equal(unname(which.max(est$posterior)), 4L)
})

test_that("uniform prior and NULL prior produce identical results", {
  uniform <- rep(0.25, 4)
  est_null    <- estimate_alpha(c(1, 0, 1), items, prior = NULL)
  est_uniform <- estimate_alpha(c(1, 0, 1), items, prior = uniform)
  expect_equal(est_null$posterior, est_uniform$posterior, tolerance = 1e-8)
})

test_that("informative prior is updated correctly by responses", {
  # Prior slightly favours [0,0] but not so strongly that all-correct
  # evidence cannot overcome it (prior 0.5, likelihood ratio >>1 for [1,1])
  p <- cdcat_prior("00" = 0.50, "10" = 0.20, "01" = 0.20, "11" = 0.10)
  est <- estimate_alpha(c(1, 1, 1), items, prior = p, method = "MAP")
  # After all-correct responses, [1,1] has much higher likelihood and wins
  expect_equal(unname(which.max(est$posterior)), 4L)
})

test_that("estimate_alpha() errors on bad prior length", {
  expect_error(
    estimate_alpha(c(1, 0, 1), items, prior = c(0.5, 0.5)),
    "length 2\\^K"
  )
})

# ---------------------------------------------------------------
# CdcatSession — prior integration
# ---------------------------------------------------------------

test_that("CdcatSession stores validated prior", {
  p <- cdcat_prior("00" = 0.4, "10" = 0.2, "01" = 0.2, "11" = 0.2)
  session <- CdcatSession$new(items, prior = p)
  expect_equal(unname(session$prior), unname(p), tolerance = 1e-8)
  expect_equal(length(session$prior), 4L)
})

test_that("CdcatSession with NULL prior stores NULL", {
  session <- CdcatSession$new(items)
  expect_null(session$prior)
})

test_that("CdcatSession errors on invalid prior at construction", {
  expect_error(
    CdcatSession$new(items, prior = c(0.5, 0.5)),
    "length 2\\^K"
  )
})

test_that("CdcatSession with informative prior runs full session", {
  p <- cdcat_prior("00" = 0.1, "10" = 0.1, "01" = 0.1, "11" = 0.7)
  session <- CdcatSession$new(items, prior = p, max_items = 3L)

  repeat {
    item <- session$next_item()
    if (item == 0) break
    session$update(item, 1L)
  }

  res <- session$result()
  expect_equal(sum(res$posterior), 1, tolerance = 1e-6)
  expect_true(res$n_items >= 1)
})

test_that("session$print() reports 'custom' prior label", {
  p <- cdcat_prior("00" = 0.4, "10" = 0.2, "01" = 0.2, "11" = 0.2)
  session <- CdcatSession$new(items, prior = p)
  out <- capture.output(session$print())
  expect_true(any(grepl("custom", out, ignore.case = TRUE)))
})

test_that("session$print() reports 'uniform' when prior is NULL", {
  session <- CdcatSession$new(items)
  out <- capture.output(session$print())
  expect_true(any(grepl("uniform", out, ignore.case = TRUE)))
})
