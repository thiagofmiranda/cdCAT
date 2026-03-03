Q <- matrix(c(1,0, 0,1, 1,1), nrow = 3, ncol = 2, byrow = TRUE)
items_dina <- cdcat_items(Q, "DINA", slip = c(0.1,0.1,0.1), guess = c(0.2,0.2,0.2))

test_that("CdcatSession initializes correctly", {
  session <- CdcatSession$new(items_dina)
  expect_true(inherits(session, "CdcatSession"))
  expect_equal(length(session$administered), 0)
  expect_equal(length(session$responses), 3)
})

test_that("next_item returns valid index", {
  session <- CdcatSession$new(items_dina)
  item <- session$next_item()
  expect_true(item %in% 1:3)
})

test_that("update registers response correctly", {
  session <- CdcatSession$new(items_dina)
  item <- session$next_item()
  session$update(item, 1)
  expect_equal(length(session$administered), 1)
  expect_equal(session$responses[item], 1)
})

test_that("session stops at max_items", {
  session <- CdcatSession$new(items_dina, max_items = 2)
  session$update(session$next_item(), 1)
  session$update(session$next_item(), 0)
  item <- session$next_item()
  expect_equal(item, 0L)
  expect_equal(session$stop_reason, "max_items reached")
})

test_that("result returns correct structure", {
  session <- CdcatSession$new(items_dina, max_items = 3)
  repeat {
    item <- session$next_item()
    if (item == 0) break
    session$update(item, 1)
  }
  res <- session$result()
  expect_true(!is.null(res$alpha_hat))
  expect_true(!is.null(res$posterior))
  expect_equal(sum(res$posterior), 1, tolerance = 1e-6)
})
