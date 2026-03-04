# tests/testthat/test-initial-profile.R

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
# Skill patterns for K=2: 00(1), 10(2), 01(3), 11(4)

# ---------------------------------------------------------------
# .validate_initial_profile()
# ---------------------------------------------------------------

test_that(".validate_initial_profile() passes for NULL", {
  expect_invisible(.validate_initial_profile(NULL, K = 2))
})

test_that(".validate_initial_profile() passes for valid binary vectors", {
  expect_invisible(.validate_initial_profile(c(0L, 0L), K = 2))
  expect_invisible(.validate_initial_profile(c(1L, 1L), K = 2))
  expect_invisible(.validate_initial_profile(c(1L, 0L), K = 2))
  expect_invisible(.validate_initial_profile(c(0, 1),   K = 2))
})

test_that(".validate_initial_profile() errors on wrong length", {
  expect_error(.validate_initial_profile(c(1L), K = 2), "length K = 2")
  expect_error(.validate_initial_profile(c(0L, 1L, 0L), K = 2), "length K = 2")
})

test_that(".validate_initial_profile() errors on non-binary values", {
  expect_error(.validate_initial_profile(c(0L, 2L), K = 2), "only 0s and 1s")
  expect_error(.validate_initial_profile(c(-1L, 1L), K = 2), "only 0s and 1s")
})

test_that(".validate_initial_profile() errors on non-numeric type", {
  expect_error(.validate_initial_profile(c("0", "1"), K = 2), "numeric or integer")
})

# ---------------------------------------------------------------
# .override_alpha_hat()
# ---------------------------------------------------------------

test_that(".override_alpha_hat() returns est unchanged when initial_profile is NULL", {
  est <- estimate_alpha(rep(NA, 5), items_5)
  out <- .override_alpha_hat(est, items_5, integer(0), NULL)
  expect_equal(out$alpha_hat_index, est$alpha_hat_index)
})

test_that(".override_alpha_hat() returns est unchanged when items have been administered", {
  est <- estimate_alpha(c(1, NA, NA, NA, NA), items_5)
  out <- .override_alpha_hat(est, items_5, c(1L), c(1L, 1L))
  expect_equal(out$alpha_hat_index, est$alpha_hat_index)
})

test_that(".override_alpha_hat() overrides alpha_hat_index correctly for [1,1]", {
  est <- estimate_alpha(rep(NA, 5), items_5)
  out <- .override_alpha_hat(est, items_5, integer(0), c(1L, 1L))
  expect_equal(out$alpha_hat_index, 4L)
})

test_that(".override_alpha_hat() overrides alpha_hat_index correctly for [0,1]", {
  est <- estimate_alpha(rep(NA, 5), items_5)
  out <- .override_alpha_hat(est, items_5, integer(0), c(0L, 1L))
  expect_equal(out$alpha_hat_index, 3L)
})

test_that(".override_alpha_hat() does not change the posterior", {
  est <- estimate_alpha(rep(NA, 5), items_5)
  posterior_before <- est$posterior
  out <- .override_alpha_hat(est, items_5, integer(0), c(1L, 1L))
  expect_equal(out$posterior, posterior_before)
})

test_that(".override_alpha_hat() errors on invalid profile", {
  est <- estimate_alpha(rep(NA, 5), items_5)
  expect_error(
    .override_alpha_hat(est, items_5, integer(0), c(2L, 0L)),
    "does not match any skill profile"
  )
})

# ---------------------------------------------------------------
# select_next_item() with initial_profile
# ---------------------------------------------------------------

test_that("select_next_item() with initial_profile returns valid item", {
  item <- select_next_item(
    items_5, rep(NA, 5), integer(0), "KL",
    initial_profile = c(1L, 1L)
  )
  expect_true(item %in% 1:5)
})

test_that("select_next_item() initial_profile is ignored after first response", {
  est_after <- estimate_alpha(c(1, NA, NA, NA, NA), items_5)

  item_no_override <- select_next_item(
    items_5, c(1, NA, NA, NA, NA), c(1L), "PWKL",
    est = est_after
  )
  item_with_override <- select_next_item(
    items_5, c(1, NA, NA, NA, NA), c(1L), "PWKL",
    est             = est_after,
    initial_profile = c(0L, 0L)
  )
  expect_equal(item_no_override, item_with_override)
})

test_that("select_next_item() initial_profile has no effect on MPWKL", {
  item_default  <- select_next_item(items_5, rep(NA, 5), integer(0), "MPWKL")
  item_override <- select_next_item(items_5, rep(NA, 5), integer(0), "MPWKL",
                                    initial_profile = c(1L, 1L))
  expect_equal(item_default, item_override)
})

test_that("select_next_item() initial_profile works in shadow mode", {
  greedy_shadow <- function(scores, items, administered) {
    scores[administered] <- -Inf
    which.max(scores)
  }
  item <- select_next_item(
    items_5, rep(NA, 5), integer(0), "KL",
    constr_fun      = greedy_shadow,
    initial_profile = c(1L, 1L)
  )
  expect_true(item %in% 1:5)
})

# ---------------------------------------------------------------
# CdcatSession with initial_profile
# ---------------------------------------------------------------

test_that("CdcatSession stores initial_profile as integer", {
  session <- CdcatSession$new(items_5, initial_profile = c(1, 1))
  expect_equal(session$initial_profile, c(1L, 1L))
})

test_that("CdcatSession with NULL initial_profile stores NULL", {
  session <- CdcatSession$new(items_5)
  expect_null(session$initial_profile)
})

test_that("CdcatSession errors on non-binary initial_profile", {
  expect_error(
    CdcatSession$new(items_5, initial_profile = c(0, 2)),
    "only 0s and 1s"
  )
})

test_that("CdcatSession errors on wrong-length initial_profile", {
  expect_error(
    CdcatSession$new(items_5, initial_profile = c(1)),
    "length K = 2"
  )
})

test_that("CdcatSession print shows initial_profile", {
  session <- CdcatSession$new(items_5, initial_profile = c(1L, 0L))
  out     <- capture.output(session$print())
  expect_true(any(grepl("10", out)))
})

test_that("CdcatSession print shows 'none' when initial_profile is NULL", {
  session <- CdcatSession$new(items_5)
  out     <- capture.output(session$print())
  expect_true(any(grepl("Init.prof.*none", out)))
})

test_that("CdcatSession runs full session with initial_profile", {
  session <- CdcatSession$new(
    items_5,
    min_items       = 5L,
    max_items       = 5L,
    initial_profile = c(1L, 1L)
  )
  resp <- c(1, 0, 1, 0, 1)
  repeat {
    item <- session$next_item()
    if (item == 0) break
    session$update(item, resp[item])
  }
  res <- session$result()
  expect_equal(res$n_items, 5L)
  expect_equal(sum(res$posterior), 1, tolerance = 1e-6)
})

test_that("initial_profile does not affect posterior after first response", {
  resp <- c(1, 0, 1, 0, 1)

  run_one_and_check_posterior <- function(prof) {
    s <- CdcatSession$new(items_5, initial_profile = prof)
    item1 <- s$next_item()
    s$update(item1, resp[item1])
    list(posterior = s$est$posterior, item1 = item1)
  }

  res_a <- run_one_and_check_posterior(c(0L, 0L))
  res_b <- run_one_and_check_posterior(c(1L, 1L))

  # If same first item was selected, posteriors must be identical
  if (res_a$item1 == res_b$item1) {
    expect_equal(res_a$posterior, res_b$posterior, tolerance = 1e-8)
  }
  # In all cases, posteriors must be valid distributions
  expect_equal(sum(res_a$posterior), 1, tolerance = 1e-6)
  expect_equal(sum(res_b$posterior), 1, tolerance = 1e-6)
})
