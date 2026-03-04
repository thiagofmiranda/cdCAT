Q <- matrix(
  c(1, 0,
    0, 1,
    1, 1,
    1, 0,
    0, 1),
  nrow = 5, ncol = 2, byrow = TRUE
)
items5 <- cdcat_items(
  Q, "DINA",
  slip  = rep(0.1, 5),
  guess = rep(0.2, 5)
)

# ----------------------------------------------------------------
# .validate_start_item
# ----------------------------------------------------------------
test_that(".validate_start_item accepts valid integer", {
  expect_invisible(.validate_start_item(1L, 5))
  expect_invisible(.validate_start_item(5L, 5))
  expect_invisible(.validate_start_item(3, 5))   # numeric coerced
})

test_that(".validate_start_item accepts valid character criteria", {
  for (cr in c("random", "seq", "KL", "PWKL", "MPWKL", "SHE",
               "RANDOM", "SEQ")) {
    expect_invisible(.validate_start_item(cr, 5))
  }
})

test_that(".validate_start_item rejects out-of-range integer", {
  expect_error(.validate_start_item(0L, 5))
  expect_error(.validate_start_item(6L, 5))
  expect_error(.validate_start_item(-1L, 5))
})

test_that(".validate_start_item rejects invalid character", {
  expect_error(.validate_start_item("INVALID", 5))
  expect_error(.validate_start_item("", 5))
})

test_that(".validate_start_item rejects non-integer/non-character", {
  expect_error(.validate_start_item(TRUE, 5))
  expect_error(.validate_start_item(list(1), 5))
})

test_that(".validate_start_item accepts NULL", {
  expect_invisible(.validate_start_item(NULL, 5))
})

# ----------------------------------------------------------------
# CdcatSession$new — parameter storage
# ----------------------------------------------------------------
test_that("start_item = 1L stored as integer", {
  s <- CdcatSession$new(items5, start_item = 1L)
  expect_identical(s$start_item, 1L)
})

test_that("start_item numeric coerced to integer", {
  s <- CdcatSession$new(items5, start_item = 3)
  expect_identical(s$start_item, 3L)
  expect_true(is.integer(s$start_item))
})

test_that("start_item character uppercased", {
  s <- CdcatSession$new(items5, start_item = "random")
  expect_identical(s$start_item, "RANDOM")
})

test_that("start_item default is NULL", {
  s <- CdcatSession$new(items5)
  expect_null(s$start_item)
})

test_that("invalid start_item stops initialization", {
  expect_error(CdcatSession$new(items5, start_item = 99L))
  expect_error(CdcatSession$new(items5, start_item = "INVALID"))
  expect_error(CdcatSession$new(items5, start_item = 0L))
})

# ----------------------------------------------------------------
# next_item — first item selection behaviour
# ----------------------------------------------------------------
test_that("start_item integer selects that exact item first", {
  for (idx in 1:5) {
    s <- CdcatSession$new(items5, start_item = as.integer(idx))
    expect_equal(s$next_item(), as.integer(idx))
  }
})

test_that("start_item 'random' returns a valid item index", {
  set.seed(42)
  s <- CdcatSession$new(items5, start_item = "random")
  item <- s$next_item()
  expect_true(item %in% 1:5)
})

test_that("start_item 'seq' returns item 1", {
  s <- CdcatSession$new(items5, start_item = "seq")
  expect_equal(s$next_item(), 1L)
})

test_that("start_item 'KL' returns a valid item index", {
  s <- CdcatSession$new(items5, start_item = "KL")
  item <- s$next_item()
  expect_true(item %in% 1:5)
})

test_that("start_item 'PWKL' returns a valid item index", {
  s <- CdcatSession$new(items5, start_item = "PWKL")
  item <- s$next_item()
  expect_true(item %in% 1:5)
})

test_that("start_item 'MPWKL' returns a valid item index", {
  s <- CdcatSession$new(items5, start_item = "MPWKL")
  item <- s$next_item()
  expect_true(item %in% 1:5)
})

test_that("start_item 'SHE' returns a valid item index", {
  s <- CdcatSession$new(items5, start_item = "SHE")
  item <- s$next_item()
  expect_true(item %in% 1:5)
})

# ----------------------------------------------------------------
# next_item — first item only; subsequent items use main criterion
# ----------------------------------------------------------------
test_that("start_item only affects first item; rest use main criterion", {
  s <- CdcatSession$new(items5, criterion = "PWKL", start_item = 3L)
  first <- s$next_item()
  expect_equal(first, 3L)

  s$update(first, 1L)

  second <- s$next_item()
  expect_true(second %in% setdiff(1:5, 3L))
})

# ----------------------------------------------------------------
# pending record for first item
# ----------------------------------------------------------------
test_that("pending record is populated after first next_item()", {
  s <- CdcatSession$new(items5, start_item = 2L)
  item <- s$next_item()
  expect_equal(item, 2L)

  # After update() the history should be recorded
  s$update(item, 0L)
  expect_equal(length(s$history), 1L)
  expect_equal(s$history[[1]]$item, 2L)
})

# ----------------------------------------------------------------
# print method includes start_item
# ----------------------------------------------------------------
test_that("print includes start item line when start_item is set (integer)", {
  s <- CdcatSession$new(items5, start_item = 2L)
  out <- capture.output(print(s))
  expect_true(any(grepl("Start", out)))
  expect_true(any(grepl("2", out)))
})

test_that("print omits start item line when start_item = NULL", {
  s <- CdcatSession$new(items5)
  out <- capture.output(print(s))
  expect_false(any(grepl("Start", out)))
})

test_that("print includes start item line (character)", {
  s <- CdcatSession$new(items5, start_item = "random")
  out <- capture.output(print(s))
  expect_true(any(grepl("RANDOM", out)))
})

# ----------------------------------------------------------------
# Reproducibility of random first item
# ----------------------------------------------------------------
test_that("start_item = 'random' is reproducible with set.seed", {
  set.seed(1)
  s1 <- CdcatSession$new(items5, start_item = "random")
  i1 <- s1$next_item()

  set.seed(1)
  s2 <- CdcatSession$new(items5, start_item = "random")
  i2 <- s2$next_item()

  expect_equal(i1, i2)
})
