#' Evaluate CD-CAT stopping criteria
#'
#' Checks whether the adaptive test should stop based on the current
#' session state.
#'
#' @param est A list returned by `estimate_alpha()`.
#' @param n_administered Integer. Number of items already administered.
#' @param min_items Integer. Minimum number of items before stopping is allowed.
#' @param max_items Integer. Maximum number of items allowed.
#' @param threshold Numeric in (0.5, 1). Posterior probability threshold for
#'   attribute classification. Stop when all attributes are classified above
#'   this threshold (mastered or not mastered). Default is `0.8`.
#'
#' @return A list with:
#'   \item{stop}{Logical. Whether the test should stop.}
#'   \item{reason}{Character string describing the stopping reason.}
#' @export
check_stopping <- function(
    est,
    n_administered,
    min_items  = 1L,
    max_items  = 20L,
    threshold  = 0.8
) {

  # Ainda não atingiu o mínimo
  if (n_administered < min_items)
    return(list(stop = FALSE, reason = "min_items not reached"))

  # Atingiu o máximo
  if (n_administered >= max_items)
    return(list(stop = TRUE, reason = "max_items reached"))

  # Classificação por threshold
  # Para cada atributo: soma a posterior de todos os perfis
  # onde aquele atributo = 1
  posterior <- est$posterior
  prob_matrix_profiles <- attr(est, "skill_patterns")

  # Verifica se todos os atributos foram classificados
  if (!is.null(prob_matrix_profiles)) {
    K <- ncol(prob_matrix_profiles)
    classified <- sapply(seq_len(K), function(k) {
      p_mastery <- sum(posterior[prob_matrix_profiles[, k] == 1])
      p_mastery >= threshold || p_mastery <= (1 - threshold)
    })

    if (all(classified))
      return(list(stop = TRUE, reason = "threshold reached"))
  }

  list(stop = FALSE, reason = "continuing")
}


#' Build skill patterns matrix
#'
#' Generates all 2^K binary skill profiles for K attributes.
#'
#' @param K Integer. Number of attributes.
#' @return A matrix of 2^K rows and K columns.
#' @export
build_skill_patterns <- function(K) {
  as.matrix(expand.grid(replicate(K, 0:1, simplify = FALSE)))
}
