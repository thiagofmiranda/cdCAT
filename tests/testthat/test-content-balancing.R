# tests/testthat/test-content-balancing.R

# --- Shared fixtures -----------------------------------------------
Q <- matrix(c(
  1, 0,
  1, 0,
  1, 0,
  0, 1,
  0, 1,
  0, 1
), nrow = 6, ncol = 2, byrow = TRUE)

items_6 <- cdcat_items(
  Q,
  "DINA",
  slip  = rep(0.1, 6),
  guess = rep(0.2, 6)
)

# Items 1-3 = "A", items 4-6 = "B", target 50/50
content_6    <- c("A", "A", "A", "B", "B", "B")
content_prop <- c(A = 0.5, B = 0.5)


# ---------------------------------------------------------------
# .validate_content() — internal validator
# ---------------------------------------------------------------

test_that(".validate_content() passes with NULL/NULL", {
  expect_invisible(.validate_content(NULL, NULL, 6L))
})

test_that(".validate_content() errors when only content supplied", {
  expect_error(
    .validate_content(content_6, NULL, 6L),
    "content_prop is NULL"
  )
})

test_that(".validate_content() errors when only content_prop supplied", {
  expect_error(
    .validate_content(NULL, content_prop, 6L),
    "content is NULL"
  )
})

test_that(".validate_content() errors on wrong content length", {
  expect_error(
    .validate_content(content_6[1:3], content_prop, 6L),
    "length equal to the number of items"
  )
})

test_that(".validate_content() errors on non-character content", {
  expect_error(
    .validate_content(1:6, content_prop, 6L),
    "character vector"
  )
})

test_that(".validate_content() errors on content_prop not summing to 1", {
  bad_prop <- c(A = 0.6, B = 0.6)
  expect_error(
    .validate_content(content_6, bad_prop, 6L),
    "sum to 1"
  )
})

test_that(".validate_content() errors on negative content_prop", {
  bad_prop <- c(A = -0.5, B = 1.5)
  expect_error(
    .validate_content(content_6, bad_prop, 6L),
    "non-negative"
  )
})

test_that(".validate_content() errors on unnamed content_prop", {
  expect_error(
    .validate_content(content_6, c(0.5, 0.5), 6L),
    "named"
  )
})

test_that(".validate_content() errors on domain in content_prop missing from content", {
  bad_prop <- c(A = 0.5, C = 0.5)
  expect_error(
    .validate_content(content_6, bad_prop, 6L),
    "not found in content"
  )
})

test_that(".validate_content() errors on domain in content missing from content_prop", {
  Q2 <- rbind(Q, c(1, 1))
  items_7 <- cdcat_items(Q2, "DINA", slip = rep(0.1,7), guess = rep(0.2,7))
  content_7 <- c(content_6, "C")   # extra domain not in content_prop
  expect_error(
    .validate_content(content_7, content_prop, 7L),
    "not listed in content_prop"
  )
})


# ---------------------------------------------------------------
# apply_content_balancing() — core logic
# ---------------------------------------------------------------

test_that("returns all candidates when no items administered", {
  result <- apply_content_balancing(1:6, integer(0), content_6, content_prop)
  expect_equal(sort(result), 1:6)
})

test_that("returns all candidates when content/content_prop are NULL", {
  result <- apply_content_balancing(1:6, c(1L, 4L), NULL, NULL)
  expect_equal(result, 1:6)
})

test_that("favours B domain after two A items administered", {
  # Administered: items 1, 2 (both A) → gap = B 0.5 - 0 = 0.5
  result <- apply_content_balancing(3:6, c(1L, 2L), content_6, content_prop)
  expect_true(all(result %in% 4:6))   # only B items
})

test_that("favours A domain after two B items administered", {
  result <- apply_content_balancing(c(1L,2L,3L,5L,6L), c(4L), content_6, content_prop)
  # 1 B out of 1 → gap = A: 0.5-0=0.5 vs B: 0.5-1=-0.5 → A wins
  expect_true(all(result %in% 1:3))
})

test_that("falls back to full candidate pool when target domain is exhausted", {
  # All B items used; only A candidates remain but target would be B
  result <- apply_content_balancing(
    candidate_items = 1:3,           # only A items left
    administered    = c(4L, 5L, 6L), # all B administered
    content         = content_6,
    content_prop    = content_prop
  )
  expect_equal(sort(result), 1:3)    # fallback: return all candidates
})

test_that("works with more than two domains", {
  content_3 <- c("X", "X", "Y", "Y", "Z", "Z")
  prop_3    <- c(X = 1/3, Y = 1/3, Z = 1/3)

  # Administered: one X, one Y → Z has 0 proportion → gap Z = 1/3
  result <- apply_content_balancing(
    candidate_items = 3:6,
    administered    = c(1L, 3L),
    content         = content_3,
    content_prop    = prop_3
  )
  expect_true(all(result %in% 5:6))  # only Z items
})


# ---------------------------------------------------------------
# select_next_item() — content balancing integrated
# ---------------------------------------------------------------

test_that("select_next_item() without content returns valid item", {
  item <- select_next_item(items_6, rep(NA, 6), integer(0), "PWKL")
  expect_true(item %in% 1:6)
})

test_that("select_next_item() with content respects domain gap", {
  # After administering two A items (1, 2), next should come from B (4-6)
  resp <- c(1, 1, NA, NA, NA, NA)
  item <- select_next_item(
    items_6, resp, c(1L, 2L), "PWKL",
    content      = content_6,
    content_prop = content_prop
  )
  expect_true(item %in% 4:6)
})

test_that("select_next_item() SEQ with content picks first item in target domain", {
  resp <- c(1, 1, NA, NA, NA, NA)
  item <- select_next_item(
    items_6, resp, c(1L, 2L), "SEQ",
    content      = content_6,
    content_prop = content_prop
  )
  expect_equal(item, 4L)   # first available B item
})


# ---------------------------------------------------------------
# CdcatSession — content balancing integration
# ---------------------------------------------------------------

test_that("CdcatSession initializes with valid content fields", {
  session <- CdcatSession$new(
    items_6, content = content_6, content_prop = content_prop
  )
  expect_equal(session$content, content_6)
  expect_equal(session$content_prop, content_prop)
})

test_that("CdcatSession with NULL content stores NULL", {
  session <- CdcatSession$new(items_6)
  expect_null(session$content)
  expect_null(session$content_prop)
})

test_that("CdcatSession errors on invalid content at construction", {
  expect_error(
    CdcatSession$new(items_6, content = content_6, content_prop = NULL),
    "content_prop is NULL"
  )
})

test_that("CdcatSession full run with content balancing completes", {
  session <- CdcatSession$new(
    items_6,
    min_items    = 6L,
    max_items    = 6L,
    content      = content_6,
    content_prop = content_prop
  )
  resp <- c(1, 0, 1, 0, 1, 0)
  repeat {
    item <- session$next_item()
    if (item == 0) break
    session$update(item, resp[item])
  }
  res <- session$result()
  expect_equal(res$n_items, 6L)
  expect_equal(sum(res$posterior), 1, tolerance = 1e-6)
})

test_that("CdcatSession with content balancing yields balanced domain counts", {
  session <- CdcatSession$new(
    items_6,
    min_items    = 6L,
    max_items    = 6L,
    content      = content_6,
    content_prop = content_prop
  )
  resp <- c(1, 0, 1, 0, 1, 0)
  repeat {
    item <- session$next_item()
    if (item == 0) break
    session$update(item, resp[item])
  }
  res      <- session$result()
  domains  <- content_6[res$administered]
  counts   <- table(domains)
  # With perfect 50/50 split over 6 items, each domain gets 3
  expect_equal(as.integer(counts["A"]), 3L)
  expect_equal(as.integer(counts["B"]), 3L)
})

test_that("session$print() reports 'active' content label", {
  session <- CdcatSession$new(
    items_6, content = content_6, content_prop = content_prop
  )
  out <- capture.output(session$print())
  expect_true(any(grepl("active", out, ignore.case = TRUE)))
})
