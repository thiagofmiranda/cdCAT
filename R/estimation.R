#' Compute item probability matrix
#'
#' Computes P(X=1 | alpha) for all items and all latent skill profiles.
#'
#' @param q_matrix Binary matrix of items x attributes.
#' @param parameters List of item parameters (slip/guess for DINA/DINO, named probs for GDINA).
#' @param skill_patterns Binary matrix of all 2^K skill profiles.
#' @param model A string: `"DINA"`, `"DINO"`, or `"GDINA"`.
#'
#' @return A matrix J x 2^K of response probabilities.
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


#' Estimate attribute profiles
#'
#' Estimates the latent skill profile (alpha) given observed responses.
#'
#' @param responses Numeric vector of 0/1/NA responses (one per item).
#' @param items A `cdcat_items` object.
#' @param method Estimation method: `"MLE"`, `"MAP"`, or `"EAP"`.
#' @param prior Numeric vector of prior probabilities over 2^K profiles.
#'   If `NULL`, uniform prior is used.
#'
#' @return A list of class `cdcat_est` with `alpha_hat`, `alpha_hat_index`,
#'   `posterior`, `prob_matrix`, and `skill_patterns`.
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
    as.numeric(prior)
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
