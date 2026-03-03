Q <- matrix(c(1,0, 0,1, 1,1), nrow = 3, ncol = 2, byrow = TRUE)
items_dina <- cdcat_items(Q, "DINA", slip = c(0.1,0.1,0.1), guess = c(0.2,0.2,0.2))
est <- estimate_alpha(c(NA, NA, NA), items_dina)

test_that("select_next_item returns valid index", {
  item <- select_next_item(items_dina, c(NA,NA,NA), integer(0), "PWKL")
  expect_true(item %in% 1:3)
})

test_that("select_next_item excludes administered items", {
  item <- select_next_item(items_dina, c(1,NA,NA), c(1L), "PWKL")
  expect_false(item == 1)
})

test_that("select_next_item returns 0 when all items administered", {
  item <- select_next_item(items_dina, c(1,1,1), c(1L,2L,3L), "PWKL")
  expect_equal(item, 0L)
})

test_that("sequential criterion returns first available item", {
  item <- select_next_item(items_dina, c(NA,NA,NA), integer(0), "SEQ")
  expect_equal(item, 1L)

  item2 <- select_next_item(items_dina, c(1,NA,NA), c(1L), "SEQ")
  expect_equal(item2, 2L)
})

test_that("random criterion returns valid index", {
  set.seed(42)
  item <- select_next_item(items_dina, c(NA,NA,NA), integer(0), "RANDOM")
  expect_true(item %in% 1:3)
})

test_that("all adaptive criteria return valid index", {
  for (crit in c("KL", "PWKL", "MPWKL", "SHE")) {
    item <- select_next_item(items_dina, c(1,NA,NA), c(1L), crit, est = est)
    expect_true(item %in% 2:3)
  }
})

test_that("unknown criterion throws error", {
  expect_error(
    select_next_item(items_dina, c(NA,NA,NA), integer(0), "INVALID"),
    "Unknown criterion"
  )
})
