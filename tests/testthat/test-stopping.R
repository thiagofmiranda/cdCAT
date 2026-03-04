Q <- matrix(c(1,0, 0,1, 1,1), nrow = 3, ncol = 2, byrow = TRUE)
items_dina <- cdcat_items(Q, "DINA", slip = c(0.1,0.1,0.1), guess = c(0.2,0.2,0.2))

est_uniform <- estimate_alpha(c(NA, NA, NA), items_dina)
est_concentrated <- estimate_alpha(c(1, 1, 1), items_dina)
est_mixed <- estimate_alpha(c(1, 0, 1), items_dina)

test_that("stops at max_items", {
  result <- check_stopping(est_uniform, n_administered = 20, max_items = 20)
  expect_true(result$stop)
  expect_equal(result$reason, "max_items reached")
})

test_that("does not stop before min_items", {
  result <- check_stopping(est_concentrated, n_administered = 0, min_items = 3)
  expect_false(result$stop)
  expect_equal(result$reason, "min_items not reached")
})

test_that("continues when posterior is uncertain", {
  result <- check_stopping(est_uniform, n_administered = 1, threshold = 0.8)
  expect_false(result$stop)
})

test_that("single threshold stops when posterior is concentrated", {
  result <- check_stopping(est_concentrated, n_administered = 3, threshold = 0.8)
  expect_true(result$stop)
  expect_equal(result$reason, "single threshold reached")
})

test_that("dual threshold stops when top is high and second is low", {
  result <- check_stopping(
    est_concentrated,
    n_administered = 3,
    threshold = c(0.7, 0.1)
  )
  expect_true(result$stop)
  expect_equal(result$reason, "dual threshold reached")
})

test_that("dual threshold does not stop when second profile is still likely", {
  result <- check_stopping(
    est_mixed,
    n_administered = 3,
    threshold = c(0.7, 0.1)
  )
  # est_mixed tem posterior [0.12, 0.55, 0.015, 0.31] — segundo perfil alto
  expect_false(result$stop)
})

test_that("threshold validation rejects invalid values", {
  expect_error(
    check_stopping(est_uniform, n_administered = 1, threshold = 1.5),
    "must be in"
  )
  expect_error(
    check_stopping(est_uniform, n_administered = 1, threshold = c(0.3, 0.8)),
    "threshold\\[1\\] must be greater than threshold\\[2\\]"
  )
  expect_error(
    check_stopping(est_uniform, n_administered = 1, threshold = c(0.8, 0.5, 0.2)),
    "length 1 or 2"
  )
})

test_that("dual threshold used in full session", {
  session <- CdcatSession$new(
    items     = items_dina,
    max_items = 10L,
    threshold = c(0.7, 0.1)
  )
  repeat {
    item <- session$next_item()
    if (item == 0) break
    session$update(item, 1)
  }
  expect_true(
    session$stop_reason %in% c("dual threshold reached", "max_items reached")
  )
})
