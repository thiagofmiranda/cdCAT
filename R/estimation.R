#' Compute Item Response Probability Matrix
#'
#' Computes the matrix of item response probabilities
#' \eqn{P(X_j = 1 | \alpha)} for all items and all latent
#' attribute profiles under the specified cognitive diagnosis model.
#'
#' @details
#' This function computes item response probabilities under three
#' cognitive diagnosis models:
#'
#' \strong{DINA model:}
#' \deqn{
#' P(X_j=1|\alpha) =
#' (1-s_j)^{\eta_j(\alpha)} g_j^{1-\eta_j(\alpha)}
#' }
#'
#' \strong{DINO model:}
#' Same functional form as DINA, but with
#' \eqn{\eta_j(\alpha) = 1} if at least one required attribute is mastered.
#'
#' \strong{GDINA model:}
#' A saturated model allowing arbitrary interaction effects among
#' required attributes.
#'
#' @param q_matrix Binary matrix of items by attributes.
#' @param parameters Model-specific item parameters.
#' @param skill_patterns Matrix of all \eqn{2^K} attribute profiles.
#' @param model Character string specifying the CDM.
#'
#' @return A matrix of dimension \eqn{J \times 2^K}.
#'
#' @export
get_prob_matrix <- function(q_matrix, parameters, skill_patterns, model) {

  model   <- toupper(model)
  J       <- nrow(q_matrix)
  K       <- ncol(q_matrix)
  n_alpha <- nrow(skill_patterns)

  if (ncol(skill_patterns) != K)
    stop("Number of attributes in q_matrix and skill_patterns do not match.")

  if (model %in% c("DINA", "DINO")) {

    slip  <- sapply(parameters, function(x) x$slip)
    guess <- sapply(parameters, function(x) x$guess)

    required_matrix   <- q_matrix %*% t(skill_patterns)
    n_required_skills <- rowSums(q_matrix)

    mastered <- if (model == "DINA") {
      required_matrix == n_required_skills
    } else {
      required_matrix > 0
    }

    return(mastered * (1 - slip) + (!mastered) * guess)
  }

  if (model == "GDINA") {

    prob_matrix <- matrix(NA_real_, nrow = J, ncol = n_alpha)

    for (j in seq_len(J)) {
      required <- q_matrix[j, ] == 1
      for (a in seq_len(n_alpha)) {
        key <- paste0(skill_patterns[a, required], collapse = "")
        if (!key %in% names(parameters[[j]]))
          stop(sprintf("Missing GDINA parameter for item %d, pattern '%s'.", j, key))
        prob_matrix[j, a] <- parameters[[j]][[key]]
      }
    }
    return(prob_matrix)
  }

  stop("Model must be 'DINA', 'DINO', or 'GDINA'.")
}


#' Estimate Latent Attribute Profiles
#'
#' Estimates the latent attribute profile given observed item responses
#' under a specified cognitive diagnosis model.
#'
#' @details
#' The likelihood for latent profile \eqn{\alpha_c} is:
#'
#' \deqn{
#' L(\alpha_c) =
#' \prod_{j \in \mathcal{J}_t}
#' P(X_j = x_j | \alpha_c)
#' }
#'
#' MLE selects:
#' \deqn{
#' \hat{\alpha}_{MLE} = \arg\max_c L(\alpha_c)
#' }
#'
#' MAP selects:
#' \deqn{
#' \hat{\alpha}_{MAP} = \arg\max_c L(\alpha_c)\pi(\alpha_c)
#' }
#'
#' EAP estimates attribute mastery probabilities:
#' \deqn{
#' \hat{\alpha}_{EAP,k} =
#' \sum_c \pi_t(\alpha_c)\alpha_{ck}
#' }
#'
#' @param responses Numeric vector of item responses.
#' @param items A \code{cdcat_items} object.
#' @param method Estimation method: "MLE", "MAP", or "EAP".
#' @param prior Prior distribution over profiles.
#'
#' @return A list of class \code{cdcat_est}.
#'
#' @export
estimate_alpha <- function(responses, items, method = "MAP", prior = NULL) {

  method <- match.arg(method, c("MLE", "MAP", "EAP"))

  Q          <- items$q_matrix
  parameters <- items$parameters
  K          <- items$n_attrs

  skill_patterns <- build_skill_patterns(K)
  n_profiles     <- nrow(skill_patterns)

  prior <- if (is.null(prior)) {
    rep(1 / n_profiles, n_profiles)
  } else {
    .validate_prior(prior, K)
  }

  prob_matrix  <- get_prob_matrix(Q, parameters, skill_patterns, items$model)
  answered_idx <- which(!is.na(responses))

  # Sem respostas — retorna prior
  if (length(answered_idx) == 0) {
    alpha_hat_index <- which.max(prior)
    return(structure(
      list(
        alpha_hat       = skill_patterns[alpha_hat_index, , drop = FALSE],
        alpha_hat_index = alpha_hat_index,
        posterior       = prior,
        prob_matrix     = prob_matrix,
        skill_patterns  = skill_patterns
      ),
      class = "cdcat_est"
    ))
  }

  resp_obs <- responses[answered_idx]
  prob_obs <- prob_matrix[answered_idx, , drop = FALSE]

  # Likelihood
  nz     <- 1e-10
  like_i <- apply(prob_obs, 2, function(p) {
    prod((p + nz)^resp_obs * (1 - p + nz)^(1 - resp_obs))
  })

  # Posterior
  post_i <- like_i * prior
  post_i <- post_i / sum(post_i)

  if (method == "MLE") {
    alpha_hat_index <- which.max(like_i)
    alpha_hat       <- skill_patterns[alpha_hat_index, , drop = FALSE]
  } else if (method == "MAP") {
    alpha_hat_index <- which.max(post_i)
    alpha_hat       <- skill_patterns[alpha_hat_index, , drop = FALSE]
  } else {
    alpha_hat_index <- which.max(post_i)
    alpha_hat       <- matrix(as.vector(post_i %*% skill_patterns), nrow = 1)
  }

  colnames(alpha_hat) <- paste0("A", seq_len(K))

  structure(
    list(
      alpha_hat       = alpha_hat,
      alpha_hat_index = alpha_hat_index,
      posterior       = post_i,
      prob_matrix     = prob_matrix,
      skill_patterns  = skill_patterns
    ),
    class = "cdcat_est"
  )
}
