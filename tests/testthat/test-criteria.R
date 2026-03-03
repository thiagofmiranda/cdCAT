Q <- matrix(c(1,0, 0,1, 1,1), nrow = 3, ncol = 2, byrow = TRUE)
items_dina <- cdcat_items(Q, "DINA", slip = c(0.1,0.1,0.1), guess = c(0.2,0.2,0.2))
est <- estimate_alpha(c(1, NA, NA), items_dina)

pm  <- est$prob_matrix
pos <- est$posterior
idx <- est$alpha_hat_index

test_that("KL_criteria returns positive numeric", {
  score <- KL_criteria(1, idx, pm)
  expect_true(is.numeric(score))
  expect_true(score >= 0)
})

test_that("PWKL_criteria returns positive numeric", {
  score <- PWKL_criteria(1, idx, pm, pos)
  expect_true(is.numeric(score))
  expect_true(score >= 0)
})

test_that("MPWKL_criteria returns positive numeric", {
  score <- MPWKL_criteria(1, pm, pos)
  expect_true(is.numeric(score))
  expect_true(score >= 0)
})

test_that("SHE_criteria returns positive numeric", {
  score <- SHE_criteria(1, pm, pos)
  expect_true(is.numeric(score))
  expect_true(score >= 0)
})

test_that("all criteria produce scores for all items", {
  for (crit in c("KL", "PWKL", "MPWKL", "SHE")) {
    scores <- sapply(1:3, function(j) {
      switch(crit,
             KL    = KL_criteria(j, idx, pm),
             PWKL  = PWKL_criteria(j, idx, pm, pos),
             MPWKL = MPWKL_criteria(j, pm, pos),
             SHE   = SHE_criteria(j, pm, pos)
      )
    })
    expect_length(scores, 3)
    expect_true(all(is.numeric(scores)))
  }
})
