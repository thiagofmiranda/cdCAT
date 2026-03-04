# tests/testthat/test-exposure-control.R

# --- Shared fixtures -----------------------------------------------
Q <- matrix(c(
  1, 0,
  1, 0,
  0, 1,
  0, 1,
  1, 1
), nrow = 5, ncol = 2, byrow = TRUE)

items_5 <- cdcat_items(
  Q, "DINA",
  slip  = rep(0.1, 5),
  guess = rep(0.2, 5)
)

scores_5    <- c(0.1, 0.5, 0.9, 0.3, 0.7)   # item 3 best, item 5 second
available_5 <- 1:5


# ---------------------------------------------------------------
# .validate_exposure() — internal validator
# ---------------------------------------------------------------

test_that(".validate_exposure() passes for NULL", {
  expect_invisible(.validate_exposure(NULL, 5L))
})

test_that(".validate_exposure() passes for valid Sympson-Hetter vector", {
  expect_invisible(.validate_exposure(rep(0.5, 5), 5L))
})

test_that(".validate_exposure() passes for valid Randomesque vector", {
  expect_invisible(.validate_exposure(rep(2, 5), 5L))
})

test_that(".validate_exposure() passes when all values are 1 (boundary)", {
  # all == 1 satisfies both [0,1] and >=1 — valid
  expect_invisible(.validate_exposure(rep(1, 5), 5L))
})

test_that(".validate_exposure() errors on wrong length", {
  expect_error(.validate_exposure(rep(0.5, 3), 5L), "length equal to")
})

test_that(".validate_exposure() errors on NA values", {
  v <- rep(0.5, 5); v[2] <- NA
  expect_error(.validate_exposure(v, 5L), "NA")
})

test_that(".validate_exposure() errors on negative values", {
  expect_error(.validate_exposure(c(-0.1, rep(0.5, 4)), 5L), "non-negative")
})

test_that(".validate_exposure() errors on mixed regime values", {
  expect_error(.validate_exposure(c(0.5, 0.5, 2, 0.5, 0.5), 5L), "Mixed")
})

test_that(".validate_exposure() errors on non-numeric input", {
  expect_error(.validate_exposure(rep("a", 5), 5L), "numeric")
})


# ---------------------------------------------------------------
# apply_sympson_hetter()
# ---------------------------------------------------------------

test_that("apply_sympson_hetter() always returns a valid global index", {
  set.seed(1)
  p <- rep(1, 5)   # always accept
  result <- apply_sympson_hetter(scores_5, available_5, p)
  expect_true(result %in% available_5)
  expect_equal(result, 3L)   # best score = item 3
})

test_that("apply_sympson_hetter() with p=0 for best item skips to next", {
  set.seed(42)
  p <- rep(1, 5)
  p[3] <- 0   # item 3 (best) always rejected
  result <- apply_sympson_hetter(scores_5, available_5, p)
  expect_false(result == 3L)
  expect_true(result %in% available_5)
})

test_that("apply_sympson_hetter() falls back with warning when all rejected", {
  # Extremely low p; force deterministic rejection via mocked runif
  set.seed(999)
  p <- rep(0, 5)   # all rejected → should warn and fall back
  expect_warning(
    result <- apply_sympson_hetter(scores_5, available_5, p),
    "Falling back"
  )
  expect_true(result %in% available_5)
})


# ---------------------------------------------------------------
# apply_randomesque()
# ---------------------------------------------------------------

test_that("apply_randomesque() with n=1 returns best item deterministically", {
  result <- apply_randomesque(scores_5, available_5, n = 1L)
  expect_equal(result, 3L)   # item 3 has highest score
})

test_that("apply_randomesque() with n=length(scores) draws from all", {
  set.seed(7)
  result <- apply_randomesque(scores_5, available_5, n = 5L)
  expect_true(result %in% available_5)
})

test_that("apply_randomesque() with n > length(scores) caps correctly", {
  set.seed(7)
  result <- apply_randomesque(scores_5, available_5, n = 99L)
  expect_true(result %in% available_5)
})

test_that("apply_randomesque() n=2 only draws from top-2 items", {
  # Top-2 by score: item 3 (0.9) and item 5 (0.7)
  results <- replicate(200, apply_randomesque(scores_5, available_5, n = 2L))
  expect_true(all(results %in% c(3L, 5L)))
})


# ---------------------------------------------------------------
# apply_exposure_control()
# ---------------------------------------------------------------

test_that("apply_exposure_control() with NULL returns best-scoring item", {
  result <- apply_exposure_control(scores_5, available_5, NULL, 0L)
  expect_equal(result, 3L)
})

test_that("apply_exposure_control() dispatches to Sympson-Hetter regime", {
  set.seed(1)
  exposure <- rep(1, 5)   # accept all
  result <- apply_exposure_control(scores_5, available_5, exposure, 2L)
  expect_equal(result, 3L)
})

test_that("apply_exposure_control() dispatches to Randomesque regime", {
  set.seed(7)
  exposure <- rep(1L, 5)
  exposure[3] <- 2L   # at position 3, draw from top-2
  result <- apply_exposure_control(scores_5, available_5, exposure, 2L)
  expect_true(result %in% c(3L, 5L))
})

test_that("apply_exposure_control() errors on mixed exposure values", {
  bad <- c(0.5, 0.5, 2, 0.5, 0.5)
  expect_error(
    apply_exposure_control(scores_5, available_5, bad, 0L),
    "Mixed"
  )
})

test_that("Randomesque falls back to greedy when k > length(exposure)", {
  exposure <- rep(2L, 3)   # shorter than n_items
  result <- apply_exposure_control(scores_5, available_5, exposure, 5L)
  # k = 6 > 3, so fallback to greedy
  expect_equal(result, 3L)
})


# ---------------------------------------------------------------
# select_next_item() — exposure control integrated
# ---------------------------------------------------------------

test_that("select_next_item() without exposure returns valid item", {
  item <- select_next_item(items_5, rep(NA, 5), integer(0), "PWKL")
  expect_true(item %in% 1:5)
})

test_that("select_next_item() with Sympson-Hetter exposure returns valid item", {
  set.seed(1)
  exposure <- rep(0.9, 5)
  item <- select_next_item(
    items_5, rep(NA, 5), integer(0), "PWKL",
    exposure = exposure
  )
  expect_true(item %in% 1:5)
})

test_that("select_next_item() with Randomesque exposure returns valid item", {
  set.seed(1)
  exposure <- rep(2L, 5)
  item <- select_next_item(
    items_5, rep(NA, 5), integer(0), "PWKL",
    exposure = exposure
  )
  expect_true(item %in% 1:5)
})


# ---------------------------------------------------------------
# CdcatSession — exposure control integration
# ---------------------------------------------------------------

test_that("CdcatSession stores exposure field", {
  exp <- rep(0.8, 5)
  session <- CdcatSession$new(items_5, exposure = exp)
  expect_equal(session$exposure, exp)
})

test_that("CdcatSession with NULL exposure stores NULL", {
  session <- CdcatSession$new(items_5)
  expect_null(session$exposure)
})

test_that("CdcatSession errors on invalid exposure at construction", {
  expect_error(
    CdcatSession$new(items_5, exposure = c(0.5, 0.5, 2, 0.5, 0.5)),
    "Mixed"
  )
})

test_that("CdcatSession with Sympson-Hetter runs full session", {
  set.seed(42)
  exposure <- rep(0.9, 5)
  session  <- CdcatSession$new(items_5, max_items = 5L, exposure = exposure)
  resp     <- c(1, 0, 1, 1, 0)
  repeat {
    item <- session$next_item()
    if (item == 0) break
    session$update(item, resp[item])
  }
  res <- session$result()
  expect_true(res$n_items >= 1)
  expect_equal(sum(res$posterior), 1, tolerance = 1e-6)
})

test_that("CdcatSession with Randomesque runs full session", {
  set.seed(42)
  exposure <- rep(2L, 5)
  session  <- CdcatSession$new(items_5, max_items = 5L, exposure = exposure)
  resp     <- c(1, 0, 1, 1, 0)
  repeat {
    item <- session$next_item()
    if (item == 0) break
    session$update(item, resp[item])
  }
  res <- session$result()
  expect_true(res$n_items >= 1)
  expect_equal(sum(res$posterior), 1, tolerance = 1e-6)
})

test_that("session$print() labels Sympson-Hetter correctly", {
  session <- CdcatSession$new(items_5, exposure = rep(0.8, 5))
  out     <- capture.output(session$print())
  expect_true(any(grepl("Sympson-Hetter", out)))
})

test_that("session$print() labels Randomesque correctly", {
  session <- CdcatSession$new(items_5, exposure = rep(2L, 5))
  out     <- capture.output(session$print())
  expect_true(any(grepl("Randomesque", out)))
})

test_that("session$print() labels 'none' when no exposure", {
  session <- CdcatSession$new(items_5)
  out     <- capture.output(session$print())
  expect_true(any(grepl("none", out)))
})
