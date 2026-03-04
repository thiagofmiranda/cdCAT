#' Create a cdCAT Item Bank
#'
#' Constructs and validates the item bank object used in cognitive
#' diagnosis computerized adaptive testing (CD-CAT) sessions.
#'
#' @details
#' The item bank is defined by a Q-matrix and model-specific item parameters.
#'
#' The Q-matrix is a binary matrix where:
#' \deqn{
#' q_{jk} =
#' \begin{cases}
#' 1 & \text{if item } j \text{ requires attribute } k, \\
#' 0 & \text{otherwise.}
#' \end{cases}
#' }
#'
#' Three cognitive diagnosis models are supported:
#'
#' \strong{DINA model}
#' (Junker & Sijtsma, 2001):
#' \deqn{
#' P(X_j=1|\alpha) =
#' (1-s_j)^{\eta_j(\alpha)}
#' g_j^{1-\eta_j(\alpha)},
#' }
#' where \eqn{s_j} is the slip parameter and \eqn{g_j} is the guess parameter.
#'
#' \strong{DINO model}
#' shares the same functional form but uses a compensatory
#' OR-type mastery indicator.
#'
#' \strong{GDINA model}
#' (de la Torre, 2011) is a saturated model allowing arbitrary
#' interaction effects among required attributes.
#'
#' All models assume:
#' \itemize{
#'   \item Binary item responses;
#'   \item Conditional independence of item responses given the latent profile;
#'   \item Fixed item parameters during the adaptive session.
#' }
#'
#' @param q_matrix A binary matrix of items (rows) by attributes (columns).
#' @param model A string: `"DINA"`, `"DINO"`, or `"GDINA"`.
#' @param slip Numeric vector of slip parameters (required for DINA/DINO).
#' @param guess Numeric vector of guess parameters (required for DINA/DINO).
#' @param gdina_params Named list of item parameters (required for GDINA).
#'
#' @return A list of class `cdcat_items` containing the Q-matrix,
#' item parameters, and model specification.
#'
#' @references
#' Junker, B. W., & Sijtsma, K. (2001).
#' Cognitive assessment models with few assumptions, and connections with
#' nonparametric item response theory.
#'
#' de la Torre, J. (2011).
#' The generalized DINA model framework.
#'
#' @export
cdcat_items <- function(
    q_matrix,
    model,
    slip         = NULL,
    guess        = NULL,
    gdina_params = NULL
) {

  model <- toupper(model)

  # --- Validações básicas
  if (!is.matrix(q_matrix))
    stop("q_matrix must be a matrix.")

  if (!all(q_matrix %in% c(0, 1)))
    stop("q_matrix must contain only 0s and 1s.")

  if (any(rowSums(q_matrix) == 0))
    stop("q_matrix contains items with no required attributes.")

  if (!model %in% c("DINA", "DINO", "GDINA"))
    stop("model must be 'DINA', 'DINO', or 'GDINA'.")

  J <- nrow(q_matrix)
  K <- ncol(q_matrix)

  # --- Parâmetros por modelo
  if (model %in% c("DINA", "DINO")) {

    if (is.null(slip) || is.null(guess))
      stop("slip and guess are required for DINA and DINO models.")

    if (length(slip) != J || length(guess) != J)
      stop("slip and guess must have length equal to number of items.")

    if (any(slip <= 0 | slip >= 1) || any(guess <= 0 | guess >= 1))
      stop("slip and guess must be in (0, 1).")

    parameters <- lapply(seq_len(J), function(i) {
      list(slip = slip[[i]], guess = guess[[i]])
    })

  } else {

    if (is.null(gdina_params))
      stop("gdina_params is required for GDINA model.")

    if (!is.list(gdina_params) || length(gdina_params) != J)
      stop("gdina_params must be a list with one entry per item.")

    parameters <- gdina_params
  }

  structure(
    list(
      q_matrix   = q_matrix,
      parameters = parameters,
      model      = model,
      n_items    = J,
      n_attrs    = K
    ),
    class = "cdcat_items"
  )
}

#' @export
print.cdcat_items <- function(x, ...) {
  cat("cdCAT Item Bank\n")
  cat("  Model  :", x$model, "\n")
  cat("  Items  :", x$n_items, "\n")
  cat("  Attrs  :", x$n_attrs, "\n")
  invisible(x)
}
