#' KL divergence criterion
#'
#' Kullback-Leibler divergence between the estimated profile and all others.
#'
#' @param item_index Integer. Index of the candidate item.
#' @param alpha_hat_index Integer. Index of the currently estimated profile.
#' @param prob_matrix Matrix J x 2^K of response probabilities.
#' @return Numeric scalar.
#' @export
KL_criteria <- function(item_index, alpha_hat_index, prob_matrix) {

  nz    <- 1e-10
  p_hat <- prob_matrix[item_index, alpha_hat_index]

  sum(sapply(seq_len(ncol(prob_matrix)), function(c) {
    p_c <- prob_matrix[item_index, c]
    p_hat * log((p_hat + nz) / (p_c + nz)) +
      (1 - p_hat) * log((1 - p_hat + nz) / (1 - p_c + nz))
  }))
}


#' PWKL criterion
#'
#' Posterior-weighted Kullback-Leibler divergence.
#'
#' @param item_index Integer. Index of the candidate item.
#' @param alpha_hat_index Integer. Index of the currently estimated profile.
#' @param prob_matrix Matrix J x 2^K of response probabilities.
#' @param posterior Numeric vector of posterior probabilities over profiles.
#' @return Numeric scalar.
#' @export
PWKL_criteria <- function(item_index, alpha_hat_index, prob_matrix, posterior) {

  nz    <- 1e-10
  p_hat <- prob_matrix[item_index, alpha_hat_index]

  sum(sapply(seq_len(ncol(prob_matrix)), function(c) {
    p_c <- prob_matrix[item_index, c]
    kl  <- p_hat * log((p_hat + nz) / (p_c + nz)) +
      (1 - p_hat) * log((1 - p_hat + nz) / (1 - p_c + nz))
    kl * posterior[c]
  }))
}


#' MPWKL criterion
#'
#' Mutual posterior-weighted Kullback-Leibler divergence.
#'
#' @param item_index Integer. Index of the candidate item.
#' @param prob_matrix Matrix J x 2^K of response probabilities.
#' @param posterior Numeric vector of posterior probabilities over profiles.
#' @return Numeric scalar.
#' @export
MPWKL_criteria <- function(item_index, prob_matrix, posterior) {

  nz <- 1e-10
  p  <- prob_matrix[item_index, ]
  n  <- length(posterior)

  total <- 0
  for (d in seq_len(n)) {
    for (c in seq_len(n)) {
      kl <- p[d] * log((p[d] + nz) / (p[c] + nz)) +
        (1 - p[d]) * log((1 - p[d] + nz) / (1 - p[c] + nz))
      total <- total + kl * posterior[d] * posterior[c]
    }
  }
  total
}


#' SHE criterion
#'
#' Shannon entropy reduction — selects the item that maximally reduces
#' posterior uncertainty.
#'
#' @param item_index Integer. Index of the candidate item.
#' @param prob_matrix Matrix J x 2^K of response probabilities.
#' @param posterior Numeric vector of posterior probabilities over profiles.
#' @return Numeric scalar (expected entropy — lower is better).
#' @export
SHE_criteria <- function(item_index, prob_matrix, posterior) {

  nz        <- 1e-10
  p_success <- prob_matrix[item_index, ]

  expected_entropy <- 0

  for (x in c(0, 1)) {
    px_given_alpha <- if (x == 1) p_success else (1 - p_success)
    prob_x         <- sum(posterior * px_given_alpha)
    posterior_x    <- (posterior * px_given_alpha) / (prob_x + nz)
    entropy_x      <- -sum(posterior_x * log(posterior_x + nz))
    expected_entropy <- expected_entropy + prob_x * entropy_x
  }

  expected_entropy
}
