#' Create a cdCAT item bank
#'
#' Builds and validates the item bank object used in CD-CAT sessions.
#'
#' @param q_matrix A binary matrix of items (rows) by attributes (columns).
#' @param model A string: `"DINA"`, `"DINO"`, or `"GDINA"`.
#' @param slip A numeric vector of slip parameters (required for DINA/DINO).
#' @param guess A numeric vector of guess parameters (required for DINA/DINO).
#' @param gdina_params A named list of item parameters (required for GDINA).
#'
#' @return A list of class `cdcat_items`.
#' @export
#'
#' @examples
#' Q <- matrix(c(1,0, 0,1, 1,1), nrow = 3, ncol = 2, byrow = TRUE)
#' items <- cdcat_items(
#'   q_matrix = Q,
#'   model    = "DINA",
#'   slip     = c(0.1, 0.1, 0.1),
#'   guess    = c(0.2, 0.2, 0.2)
#' )
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
