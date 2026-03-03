Q <- matrix(c(1,0, 0,1, 1,1), nrow = 3, ncol = 2, byrow = TRUE)
items_dina <- cdcat_items(Q, "DINA", slip = c(0.1,0.1,0.1), guess = c(0.2,0.2,0.2))

test_that("estimate_alpha returns cdcat_est object", {
  est <- estimate_alpha(c(1, 0, 1), items_dina)
  expect_s3_class(est, "cdcat_est")
})

test_that("estimate_alpha returns correct fields", {
  est <- estimate_alpha(c(1, 0, 1), items_dina)
  expect_true(!is.null(est$alpha_hat))
  expect_true(!is.null(est$posterior))
  expect_true(!is.null(est$prob_matrix))
  expect_true(!is.null(est$skill_patterns))
})

test_that("posterior sums to 1", {
  est <- estimate_alpha(c(1, 0, 1), items_dina)
  expect_equal(sum(est$posterior), 1, tolerance = 1e-6)
})

test_that("estimate_alpha with no responses returns uniform prior", {
  est <- estimate_alpha(c(NA, NA, NA), items_dina)
  expect_equal(est$posterior, rep(0.25, 4), tolerance = 1e-6)
})

test_that("all-correct responses push posterior toward [1,1]", {
  est <- estimate_alpha(c(1, 1, 1), items_dina)
  # perfil [1,1] é o último (índice 4 em expand.grid)
  expect_equal(which.max(est$posterior), 4L)
})

test_that("all-wrong responses push posterior toward [0,0]", {
  est <- estimate_alpha(c(0, 0, 0), items_dina)
  # perfil [0,0] é o primeiro
  expect_equal(which.max(est$posterior), 1L)
})
