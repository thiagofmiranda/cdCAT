#' Kullback–Leibler (KL) Information Criterion for CD-CAT
#'
#' Computes the Kullback–Leibler (KL) information index for a candidate item
#' under a cognitive diagnosis computerized adaptive testing (CD-CAT) framework.
#'
#' This criterion selects the item that maximizes the Kullback–Leibler divergence
#' between the item response distribution induced by the current estimated
#' latent profile and the response distributions induced by all alternative
#' latent profiles.
#'
#' @details
#' Let \eqn{\hat{\alpha}^{(t)}} denote the current latent profile estimate after
#' \eqn{t} administered items, and let \eqn{\alpha_c}, \eqn{c = 1,\dots,2^K},
#' denote all possible latent attribute profiles.
#'
#' For a candidate item \eqn{h}, the KL index is defined as:
#'
#' \deqn{
#' KL_h(\hat{\alpha}^{(t)}) =
#' \sum_{c=1}^{2^K}
#' \sum_{x=0}^{1}
#' P(X_h = x \mid \hat{\alpha}^{(t)})
#' \log
#' \frac{P(X_h = x \mid \hat{\alpha}^{(t)})}
#'      {P(X_h = x \mid \alpha_c)}
#' }
#'
#' Thus, the divergence is computed between conditional response distributions
#' for the candidate item under different latent profiles, not between the
#' attribute vectors themselves.
#'
#' The logarithm is computed using the natural logarithm.
#'
#' Assumptions:
#' \itemize{
#'   \item Conditional independence of item responses.
#'   \item A fixed cognitive diagnosis model defining \eqn{P(X=1|\alpha)}.
#'   \item Equal weighting of all latent profiles.
#' }
#'
#' Computational complexity per candidate item is \eqn{O(2^K)}.
#'
#' @param item_index Integer. Index of the candidate item.
#' @param alpha_hat_index Integer. Index of the current latent profile estimate.
#' @param prob_matrix Numeric matrix of dimension J x 2^K
#'   containing item response probabilities P(X=1 | alpha).
#'
#' @return Numeric scalar representing the KL information value.
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


#' Posterior-Weighted Kullback–Leibler (PWKL) Criterion
#'
#' Computes the posterior-weighted KL information index for a candidate item
#' in CD-CAT.
#'
#' Unlike the standard KL method, this criterion weights each latent profile
#' by its current posterior probability rather than assuming equally likely
#' latent states.
#'
#' @details
#' Let \eqn{\pi_t(\alpha_c)} denote the posterior probability of latent
#' profile \eqn{\alpha_c} after \eqn{t} administered items.
#'
#' The PWKL index for item \eqn{h} is defined as:
#'
#' \deqn{
#' PWKL_h(\hat{\alpha}^{(t)}) =
#' \sum_{c=1}^{2^K}
#' KL_h(\hat{\alpha}^{(t)} \Vert \alpha_c)
#' \pi_t(\alpha_c)
#' }
#'
#' where \eqn{KL_h(\cdot)} represents the KL divergence between
#' the conditional response distributions induced by two latent profiles.
#'
#' This weighting reduces early-stage instability and avoids the implicit
#' assumption of uniformly distributed latent states.
#'
#' Assumptions:
#' \itemize{
#'   \item Correct posterior updating.
#'   \item Conditional independence of responses.
#' }
#'
#' Computational complexity per candidate item is \eqn{O(2^K)}.
#'
#' @param item_index Integer. Index of the candidate item.
#' @param alpha_hat_index Integer. Index of the current latent profile estimate.
#' @param prob_matrix Numeric matrix of dimension J x 2^K
#'   containing item response probabilities.
#' @param posterior Numeric vector of posterior probabilities over
#'   the 2^K latent profiles.
#'
#' @return Numeric scalar representing the PWKL information value.
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


#' Modified Posterior-Weighted Kullback–Leibler (MPWKL) Criterion
#'
#' Computes the symmetric posterior-weighted KL divergence across all
#' pairs of item response distributions induced by latent profiles.
#'
#' @details
#' The MPWKL index for item \eqn{h} is defined as:
#'
#' \deqn{
#' MPWKL_h =
#' \sum_{d=1}^{2^K}
#' \sum_{c=1}^{2^K}
#' KL_h(\alpha_d \Vert \alpha_c)
#' \pi_t(\alpha_d)\pi_t(\alpha_c)
#' }
#'
#' where \eqn{KL_h(\alpha_d \Vert \alpha_c)} denotes the KL divergence
#' between the conditional response distributions of item \eqn{h}
#' under latent profiles \eqn{\alpha_d} and \eqn{\alpha_c}.
#'
#' This formulation evaluates the expected global discrimination of the item
#' under the full posterior distribution.
#'
#' Computational complexity per candidate item is \eqn{O((2^K)^2)}.
#'
#' @param item_index Integer. Index of the candidate item.
#' @param prob_matrix Numeric matrix of dimension J x 2^K
#'   containing item response probabilities.
#' @param posterior Numeric vector of posterior probabilities.
#'
#' @return Numeric scalar representing the MPWKL information value.
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


#' Shannon Entropy (SHE) Criterion for CD-CAT
#'
#' Computes the expected posterior Shannon entropy after administering
#' a candidate item.
#'
#' The selected item minimizes the expected posterior entropy after
#' observing the potential response to the candidate item.
#'
#' @details
#' Let \eqn{\pi_t(\alpha_c)} denote the posterior distribution over
#' latent profiles after \eqn{t} administered items.
#'
#' Shannon entropy is defined as:
#'
#' \deqn{
#' SH(\pi_t) =
#' -\sum_{c=1}^{2^K}
#' \pi_t(\alpha_c)\log \pi_t(\alpha_c)
#' }
#'
#' For candidate item \eqn{h}, the expected posterior entropy is:
#'
#' \deqn{
#' E[SH(\pi_{t+1})] =
#' \sum_{x=0}^{1}
#' SH(\pi_{t+1} \mid X_h = x)
#' P(X_h = x \mid x^{(t)})
#' }
#'
#' Thus, the criterion evaluates the expected reduction in uncertainty
#' about the latent profile induced by administering item \eqn{h}.
#'
#' Computational complexity per candidate item is \eqn{O(2^K)}.
#'
#' @param item_index Integer. Index of the candidate item.
#' @param prob_matrix Numeric matrix of dimension J x 2^K
#'   containing item response probabilities.
#' @param posterior Numeric vector of posterior probabilities.
#'
#' @return Numeric scalar representing the expected posterior entropy.
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
