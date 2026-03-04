# shadow_cat.R — Shadow CAT (teste sombra) para CD-CAT
#
# Funções exportadas : (nenhuma — lógica encapsulada em select_next_item)
# Funções internas   : .apply_shadow_cat()
#                      .validate_constr_fun()


# ------------------------------------------------------------------
# Motor interno do Shadow CAT
# ------------------------------------------------------------------

#' Apply shadow CAT item selection (internal)
#'
#' In shadow mode the full item bank is considered at every step.
#' Criterion scores are computed for **all J items** (including already
#' administered ones) and then handed to the user-supplied
#' `constr_fun`, which is responsible for enforcing any linear or
#' combinatorial constraints and returning the index of the next item.
#'
#' This design keeps `cdCAT` free of LP-solver dependencies: the user
#' can call any solver they wish (e.g. `lpSolve::lp()`,
#' `ROI::ROI_solve()`) inside their `constr_fun`.
#'
#' @param items A `cdcat_items` object.
#' @param est A `cdcat_est` object returned by [estimate_alpha()].
#' @param administered Integer vector of already administered item
#'   indices.
#' @param criterion Upper-cased criterion string.
#' @param constr_fun A function with signature
#'   `function(scores, items, administered)` that returns a single
#'   integer — the global index of the next item to administer.
#'
#' @return Integer scalar — global index of the selected item.
#' @keywords internal
.apply_shadow_cat <- function(items, est, administered, criterion, constr_fun) {

  all_items <- seq_len(items$n_items)

  prob_matrix     <- est$prob_matrix
  posterior       <- est$posterior
  alpha_hat_index <- est$alpha_hat_index

  # Non-adaptive criteria have no meaningful scores over the full bank.
  # Fall back to greedy selection over non-administered items.
  if (criterion %in% c("SEQ", "RANDOM")) {
    available <- setdiff(all_items, administered)
    if (length(available) == 0) return(0L)
    item <- if (criterion == "SEQ") available[1L] else available[sample(length(available), 1L)]
    return(as.integer(item))
  }

  # Scores for the FULL bank (shadow test considers all items)
  scores <- sapply(all_items, function(j) {
    switch(
      criterion,
      KL    = KL_criteria(j, alpha_hat_index, prob_matrix),
      PWKL  = PWKL_criteria(j, alpha_hat_index, prob_matrix, posterior),
      MPWKL = MPWKL_criteria(j, prob_matrix, posterior),
      SHE   = -SHE_criteria(j, prob_matrix, posterior),
      stop(sprintf("Unknown criterion for shadow CAT: '%s'.", criterion))
    )
  })

  # Delegate selection to the user's constraint function
  item <- constr_fun(scores, items, administered)

  # Validate the returned value
  if (!is.numeric(item) || length(item) != 1L)
    stop("constr_fun() must return a single numeric item index.")

  item <- as.integer(item)

  if (is.na(item) || item < 1L || item > items$n_items)
    stop(sprintf(
      "constr_fun() returned an out-of-range index (%s). Must be in 1:%d.",
      item, items$n_items
    ))

  item
}


# ------------------------------------------------------------------
# Validação interna
# ------------------------------------------------------------------

#' Validate a shadow CAT constraint function (internal)
#'
#' Called by [CdcatSession] at initialization to verify that
#' `constr_fun` is a proper function before the session starts.
#'
#' @param constr_fun A function or `NULL`.
#'
#' @return Invisibly `TRUE`. Stops with an informative message on error.
#' @keywords internal
.validate_constr_fun <- function(constr_fun) {

  if (is.null(constr_fun))
    return(invisible(TRUE))

  if (!is.function(constr_fun))
    stop("constr_fun must be a function or NULL.")

  # Check expected formal arguments
  args <- names(formals(constr_fun))
  expected <- c("scores", "items", "administered")

  missing_args <- setdiff(expected, args)
  if (length(missing_args) > 0)
    stop(sprintf(
      "constr_fun must accept arguments: %s. Missing: %s.",
      paste(expected, collapse = ", "),
      paste(missing_args, collapse = ", ")
    ))

  invisible(TRUE)
}
