# prior.R — Distribuição a priori para perfis de habilidade
#
# Funções exportadas : cdcat_prior()
# Funções internas   : .validate_prior(), .build_pattern_labels()


#' Build a named prior distribution over skill profiles
#'
#' Constructs and validates a prior probability vector over all 2^K
#' latent skill profiles, using pattern names as keys.
#' The pattern names follow the same ordering as [build_skill_patterns()]:
#' `expand.grid` with the first attribute varying fastest.
#'
#' @param ... Named numeric values. Each name must be a binary string of
#'   length K (e.g. `"00"`, `"01"`, `"10"`, `"11"` for K = 2).
#'   All 2^K patterns must be supplied.
#' @param K Integer. Number of attributes. Inferred from pattern length
#'   if not supplied.
#' @param normalize Logical. If `TRUE` (default), the vector is
#'   automatically normalized to sum to 1 with a warning when needed.
#'
#' @return A named numeric vector of length 2^K whose entries sum to 1,
#'   ordered consistently with [build_skill_patterns()].
#'
#' @seealso [estimate_alpha()], [CdcatSession]
#'
#' @export
#'
#' @examples
#' # K = 2: four profiles — "00", "10", "01", "11"
#' prior <- cdcat_prior("00" = 0.4, "10" = 0.2, "01" = 0.2, "11" = 0.2)
#'
#' # Use in a session
#' Q <- matrix(c(1,0, 0,1, 1,1), nrow = 3, ncol = 2, byrow = TRUE)
#' items <- cdcat_items(Q, "DINA", slip = c(0.1,0.1,0.1), guess = c(0.2,0.2,0.2))
#' session <- CdcatSession$new(items, prior = prior)
cdcat_prior <- function(..., K = NULL, normalize = TRUE) {

  raw <- list(...)

  if (length(raw) == 0)
    stop("cdcat_prior() requires at least one named probability value.")

  if (is.null(names(raw)) || any(nchar(names(raw)) == 0))
    stop("All arguments to cdcat_prior() must be named with binary pattern strings (e.g. '00', '01').")

  # --- Infer K from pattern length (must be consistent)
  pattern_lengths <- nchar(names(raw))
  if (length(unique(pattern_lengths)) != 1)
    stop("All pattern names must have the same length (number of attributes K).")

  k_inferred <- unique(pattern_lengths)

  if (!is.null(K)) {
    if (K != k_inferred)
      stop(sprintf(
        "Supplied K = %d does not match pattern length %d.",
        K, k_inferred
      ))
  }
  K <- k_inferred

  # --- Build expected patterns in expand.grid order
  expected_patterns <- .build_pattern_labels(K)
  expected_n        <- 2^K

  # --- Check extra patterns first (invalid names), then missing
  extra_pats <- setdiff(names(raw), expected_patterns)
  if (length(extra_pats) > 0)
    stop(paste(
      "Invalid pattern names (not valid binary strings for K =", K, "):",
      paste(extra_pats, collapse = ", ")
    ))

  missing_pats <- setdiff(expected_patterns, names(raw))
  if (length(missing_pats) > 0)
    stop(paste(
      "Missing prior probabilities for patterns:",
      paste(missing_pats, collapse = ", ")
    ))

  # --- Reorder to canonical expand.grid order
  prior_vec <- vapply(
    expected_patterns,
    function(p) as.numeric(raw[[p]]),
    numeric(1)
  )
  names(prior_vec) <- expected_patterns

  # --- Numeric validation
  if (any(is.na(prior_vec)))
    stop("prior contains NA values.")

  if (any(prior_vec < 0))
    stop("prior must contain non-negative values.")

  if (all(prior_vec == 0))
    stop("prior must contain at least one positive value.")

  # --- Normalize
  s <- sum(prior_vec)
  if (abs(s - 1) > 1e-8) {
    if (normalize) {
      warning(sprintf(
        "prior probabilities sum to %.6g, not 1. Normalizing automatically.",
        s
      ))
      prior_vec <- prior_vec / s
    } else {
      stop(sprintf(
        "prior probabilities must sum to 1 (got %.6g). Set normalize = TRUE to auto-normalize.",
        s
      ))
    }
  }

  prior_vec
}


#' Validate a prior probability vector (internal)
#'
#' Used by [estimate_alpha()] and [CdcatSession] to validate any
#' prior vector before use, regardless of how it was constructed.
#' Names are preserved if present.
#'
#' @param prior Numeric vector of length 2^K, or `NULL`.
#' @param K Integer. Number of attributes.
#' @param normalize Logical. Auto-normalize if sum != 1.
#'
#' @return Validated (and possibly normalized) numeric vector, or `NULL`.
#' @keywords internal
.validate_prior <- function(prior, K, normalize = TRUE) {

  if (is.null(prior))
    return(NULL)

  expected_n <- 2^K

  prior_names <- names(prior)

  # --- Type coercion
  prior <- tryCatch(
    as.numeric(prior),
    warning = function(w) stop("prior could not be coerced to numeric.")
  )

  # Restore names after coercion
  if (!is.null(prior_names))
    names(prior) <- prior_names

  # --- Length
  if (length(prior) != expected_n)
    stop(sprintf(
      "prior must have length 2^K = %d for K = %d attributes. Got length %d.",
      expected_n, K, length(prior)
    ))

  # --- NAs
  if (any(is.na(prior)))
    stop("prior contains NA values.")

  # --- Non-negative
  if (any(prior < 0))
    stop("prior must contain non-negative values.")

  # --- Not all zero
  if (all(prior == 0))
    stop("prior must contain at least one positive value.")

  # --- Normalize
  s <- sum(prior)
  if (abs(s - 1) > 1e-8) {
    if (normalize) {
      warning(sprintf(
        "prior probabilities sum to %.6g, not 1. Normalizing automatically.",
        s
      ))
      prior <- prior / s
    } else {
      stop(sprintf(
        "prior probabilities must sum to 1 (got %.6g).",
        s
      ))
    }
  }

  prior
}


#' Build canonical pattern label vector (internal)
#'
#' Returns the 2^K binary pattern strings in `expand.grid` order,
#' matching [build_skill_patterns()].
#'
#' @param K Integer. Number of attributes.
#' @return Character vector of length 2^K.
#' @keywords internal
.build_pattern_labels <- function(K) {
  sp <- as.matrix(expand.grid(replicate(K, 0:1, simplify = FALSE)))
  apply(sp, 1, paste0, collapse = "")
}
