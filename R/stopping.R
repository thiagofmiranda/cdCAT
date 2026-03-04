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


#' Evaluate CD-CAT Stopping Criteria
#'
#' Evaluates whether the computerized adaptive test should terminate
#' based on the current posterior distribution over latent profiles
#' and operational constraints.
#'
#' @details
#' This function implements three stopping mechanisms within a
#' cognitive diagnosis computerized adaptive testing (CD-CAT) framework:
#'
#' \strong{1. Fixed-length rule}
#'
#' The test stops when the number of administered items reaches
#' \code{max_items}. This corresponds to the traditional fixed-length
#' stopping rule described in the CD-CAT literature.
#'
#' \strong{2. Attribute-level posterior threshold rule}
#' (Tatsuoka, 2002)
#'
#' Let \eqn{\pi_t(\alpha_c)} denote the posterior distribution over
#' latent profiles after \eqn{t} administered items.
#'
#' The marginal posterior probability of mastery for attribute \eqn{k} is:
#'
#' \deqn{
#' P(\alpha_k = 1 \mid x^{(t)}) =
#' \sum_{c: \alpha_{ck}=1} \pi_t(\alpha_c).
#' }
#'
#' The test stops when all attributes are classified with sufficient
#' confidence:
#'
#' \deqn{
#' P(\alpha_k = 1 \mid x^{(t)}) \ge \tau
#' \quad \text{or} \quad
#' P(\alpha_k = 1 \mid x^{(t)}) \le 1 - \tau
#' \quad \text{for all } k.
#' }
#'
#' This rule performs classification at the attribute level rather than
#' at the full latent class level.
#'
#' \strong{3. Dual posterior threshold rule}
#' (Hsu, Wang, & Chen, 2013)
#'
#' Let \eqn{\pi_{(1)}} and \eqn{\pi_{(2)}} denote the largest and
#' second-largest posterior probabilities among all latent profiles.
#'
#' The test stops when:
#'
#' \deqn{
#' \pi_{(1)} \ge \tau_1
#' \quad \text{and} \quad
#' \pi_{(2)} \le \tau_2,
#' }
#'
#' where \eqn{\tau_1 > \tau_2}. This conservative rule avoids stopping
#' when two latent profiles remain similarly probable.
#'
#' In addition to these theoretical criteria, the function enforces:
#' \itemize{
#'   \item A minimum number of administered items (\code{min_items});
#'   \item A maximum number of administered items (\code{max_items}).
#' }
#'
#' All posterior computations assume conditional independence of item
#' responses given the latent profile.
#'
#' @param est A list of class \code{cdcat_est} returned by
#'   \code{estimate_alpha()}.
#' @param n_administered Integer. Number of items already administered.
#' @param min_items Integer. Minimum number of items before stopping
#'   is allowed.
#' @param max_items Integer. Maximum number of items allowed.
#' @param threshold Numeric vector of length 1 or 2:
#'   \itemize{
#'     \item Length 1: attribute-level posterior threshold \eqn{\tau}.
#'     \item Length 2: dual class-level thresholds
#'       \eqn{(\tau_1, \tau_2)}.
#'   }
#'
#' @return A list with:
#' \itemize{
#'   \item \code{stop}: Logical. Whether the test should stop.
#'   \item \code{reason}: Character string describing the stopping condition.
#' }
#'
#' @references
#' Tatsuoka, C. (2002).
#' Data analytic methods for latent partially ordered classification models.
#'
#' Hsu, C. L., Wang, W. C., & Chen, S. Y. (2013).
#' Variable-length computerized adaptive testing based on cognitive diagnosis models.
#'
#' Rupp, A. A., Templin, J., & Henson, R. (2010).
#' Diagnostic Measurement: Theory, Methods, and Applications.
#'
#' @export
check_stopping <- function(
    est,
    n_administered,
    min_items = 1L,
    max_items = 20L,
    threshold = 0.8
) {

  # --- Validação do threshold
  if (!is.numeric(threshold) || !length(threshold) %in% c(1L, 2L))
    stop("threshold must be a numeric vector of length 1 or 2.")

  if (any(threshold <= 0 | threshold >= 1))
    stop("threshold values must be in (0, 1).")

  if (length(threshold) == 2 && threshold[1] <= threshold[2])
    stop("For dual threshold, threshold[1] must be greater than threshold[2].")

  # --- Ainda não atingiu o mínimo
  if (n_administered < min_items)
    return(list(stop = FALSE, reason = "min_items not reached"))

  # --- Atingiu o máximo
  if (n_administered >= max_items)
    return(list(stop = TRUE, reason = "max_items reached"))

  # --- Regra posterior
  posterior       <- est$posterior
  skill_patterns  <- est$skill_patterns

  if (is.null(posterior) || is.null(skill_patterns))
    return(list(stop = FALSE, reason = "continuing"))

  K <- ncol(skill_patterns)

  # Probabilidade de domínio por atributo
  p_mastery <- sapply(seq_len(K), function(k) {
    sum(posterior[skill_patterns[, k] == 1])
  })

  # Ordenar posterior para regras de threshold
  ord <- sort(posterior, decreasing = TRUE)

  if (length(threshold) == 1) {

    # --- Regra 1: Single threshold (Tatsuoka, 2002; Hsu et al., 2013)
    # Todos os atributos classificados com confiança >= threshold
    classified <- all(p_mastery >= threshold | p_mastery <= (1 - threshold))

    if (classified)
      return(list(stop = TRUE, reason = "single threshold reached"))

  } else {

    # --- Regra 2: Dual threshold (Hsu et al., 2013)
    # Maior posterior >= threshold[1] E segunda maior <= threshold[2]
    second <- if (length(ord) >= 2) ord[2] else 0

    if (ord[1] >= threshold[1] && second <= threshold[2])
      return(list(stop = TRUE, reason = "dual threshold reached"))
  }

  list(stop = FALSE, reason = "continuing")
}
