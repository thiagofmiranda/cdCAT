# tests/testthat/test-shadow-cat.R

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

# Simple greedy shadow function: picks highest-scoring non-administered item
greedy_shadow <- function(scores, items, administered) {
  scores[administered] <- -Inf
  which.max(scores)
}

# Shadow function that forces item 3 to always be selected first
force_item3 <- function(scores, items, administered) {
  if (!3L %in% administered) return(3L)
  scores[administered] <- -Inf
  which.max(scores)
}

# Shadow function that restricts to items 1–3 only
restrict_to_first3 <- function(scores, items, administered) {
  allowed <- setdiff(1:3, administered)
  if (length(allowed) == 0) {
    # fallback: any non-administered item
    all_avail <- setdiff(seq_len(items$n_items), administered)
    return(all_avail[1])
  }
  allowed[which.max(scores[allowed])]
}


# ---------------------------------------------------------------
# .validate_constr_fun() — internal validator
# ---------------------------------------------------------------

test_that(".validate_constr_fun() passes for NULL", {
  expect_invisible(.validate_constr_fun(NULL))
})

test_that(".validate_constr_fun() passes for valid function", {
  expect_invisible(.validate_constr_fun(greedy_shadow))
})

test_that(".validate_constr_fun() errors when not a function", {
  expect_error(.validate_constr_fun("not_a_function"), "must be a function")
})

test_that(".validate_constr_fun() errors on missing required arguments", {
  bad_fn <- function(scores) which.max(scores)
  expect_error(.validate_constr_fun(bad_fn), "Missing")
})

test_that(".validate_constr_fun() errors on completely wrong signature", {
  bad_fn <- function(x, y) x + y
  expect_error(.validate_constr_fun(bad_fn), "Missing")
})


# ---------------------------------------------------------------
# .apply_shadow_cat() — internal engine
# ---------------------------------------------------------------

test_that(".apply_shadow_cat() returns a valid item index", {
  est  <- estimate_alpha(rep(NA, 5), items_5)
  item <- .apply_shadow_cat(items_5, est, integer(0), "PWKL", greedy_shadow)
  expect_true(item %in% 1:5)
})

test_that(".apply_shadow_cat() scores all J items (not just unadministered)", {
  # Administer items 1 and 2; greedy_shadow should still consider 3-5
  est  <- estimate_alpha(c(1, 0, NA, NA, NA), items_5)
  item <- .apply_shadow_cat(items_5, est, c(1L, 2L), "PWKL", greedy_shadow)
  expect_true(item %in% 3:5)   # administered excluded by constr_fun
})

test_that(".apply_shadow_cat() respects force_item3 constraint", {
  est  <- estimate_alpha(rep(NA, 5), items_5)
  item <- .apply_shadow_cat(items_5, est, integer(0), "PWKL", force_item3)
  expect_equal(item, 3L)
})

test_that(".apply_shadow_cat() errors when constr_fun returns wrong length", {
  bad_fn <- function(scores, items, administered) c(1L, 2L)
  est    <- estimate_alpha(rep(NA, 5), items_5)
  expect_error(
    .apply_shadow_cat(items_5, est, integer(0), "PWKL", bad_fn),
    "single numeric"
  )
})

test_that(".apply_shadow_cat() errors when constr_fun returns out-of-range index", {
  bad_fn <- function(scores, items, administered) 99L
  est    <- estimate_alpha(rep(NA, 5), items_5)
  expect_error(
    .apply_shadow_cat(items_5, est, integer(0), "PWKL", bad_fn),
    "out-of-range"
  )
})

test_that(".apply_shadow_cat() works with all adaptive criteria", {
  est <- estimate_alpha(c(1, NA, NA, NA, NA), items_5)
  for (crit in c("KL", "PWKL", "MPWKL", "SHE")) {
    item <- .apply_shadow_cat(items_5, est, c(1L), crit, greedy_shadow)
    expect_true(item %in% 2:5)
  }
})


# ---------------------------------------------------------------
# select_next_item() — shadow mode integration
# ---------------------------------------------------------------

test_that("select_next_item() in shadow mode returns valid item", {
  item <- select_next_item(
    items_5, rep(NA, 5), integer(0), "PWKL",
    constr_fun = greedy_shadow
  )
  expect_true(item %in% 1:5)
})

test_that("select_next_item() shadow mode bypasses content balancing", {
  content      <- c("A", "A", "A", "B", "B")
  content_prop <- c(A = 0.2, B = 0.8)

  # After 2 A items, greedy would force B; shadow ignores content and
  # just uses greedy_shadow (which picks highest-scoring non-administered)
  item <- select_next_item(
    items_5, c(1, 1, NA, NA, NA), c(1L, 2L), "PWKL",
    content      = content,
    content_prop = content_prop,
    constr_fun   = greedy_shadow
  )
  # constr_fun picks best of {3,4,5} — content not enforced
  expect_true(item %in% 3:5)
})

test_that("select_next_item() shadow mode warns for SEQ criterion", {
  # SEQ in shadow mode: warning emitted, then falls back to first available item
  expect_warning(
    item <- select_next_item(
      items_5, rep(NA, 5), integer(0), "SEQ",
      constr_fun = greedy_shadow
    ),
    "does not produce meaningful scores"
  )
  expect_true(item %in% 1:5)
})

test_that("select_next_item() restrict_to_first3 only returns items 1-3", {
  resp <- c(1, NA, NA, NA, NA)
  item <- select_next_item(
    items_5, resp, c(1L), "PWKL",
    constr_fun = restrict_to_first3
  )
  expect_true(item %in% 2:3)
})


# ---------------------------------------------------------------
# CdcatSession — shadow CAT integration
# ---------------------------------------------------------------

test_that("CdcatSession stores constr_fun field", {
  session <- CdcatSession$new(items_5, constr_fun = greedy_shadow)
  expect_true(is.function(session$constr_fun))
})

test_that("CdcatSession with NULL constr_fun stores NULL", {
  session <- CdcatSession$new(items_5)
  expect_null(session$constr_fun)
})

test_that("CdcatSession errors on invalid constr_fun", {
  expect_error(
    CdcatSession$new(items_5, constr_fun = "not_a_function"),
    "must be a function"
  )
})

test_that("CdcatSession warns when shadow + content/exposure supplied", {
  expect_warning(
    CdcatSession$new(
      items_5,
      constr_fun = greedy_shadow,
      exposure   = rep(0.9, 5)
    ),
    "Shadow CAT.*ignored"
  )
})

test_that("CdcatSession shadow mode runs full session", {
  session <- CdcatSession$new(
    items_5,
    min_items  = 5L,
    max_items  = 5L,
    constr_fun = greedy_shadow
  )
  resp <- c(1, 0, 1, 1, 0)
  repeat {
    item <- session$next_item()
    if (item == 0) break
    session$update(item, resp[item])
  }
  res <- session$result()
  expect_equal(res$n_items, 5L)
  expect_equal(sum(res$posterior), 1, tolerance = 1e-6)
  # Each item administered exactly once
  expect_equal(length(unique(res$administered)), 5L)
})

test_that("CdcatSession shadow mode with force_item3 always selects item 3 first", {
  session <- CdcatSession$new(
    items_5,
    max_items  = 3L,
    constr_fun = force_item3
  )
  item1 <- session$next_item()
  expect_equal(item1, 3L)
})

test_that("CdcatSession shadow mode with restrict_to_first3 only uses items 1-3", {
  session <- CdcatSession$new(
    items_5,
    max_items  = 3L,
    constr_fun = restrict_to_first3
  )
  resp <- c(1, 0, 1, 1, 0)
  repeat {
    item <- session$next_item()
    if (item == 0) break
    session$update(item, resp[item])
  }
  res <- session$result()
  expect_true(all(res$administered %in% 1:3))
})

test_that("session$print() reports shadow 'yes'", {
  session <- CdcatSession$new(items_5, constr_fun = greedy_shadow)
  out     <- capture.output(session$print())
  expect_true(any(grepl("yes", out)))
})

test_that("session$print() reports shadow 'no' without constr_fun", {
  session <- CdcatSession$new(items_5)
  out     <- capture.output(session$print())
  expect_true(any(grepl("Shadow.*no", out)))
})
