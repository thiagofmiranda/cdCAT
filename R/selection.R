#' Select the next item in a CD-CAT session
#'
#' Computes the item selection criterion score for all available items
#' and returns the index of the best candidate.
#'
#' **Greedy mode** (`constr_fun = NULL`, default):
#' \enumerate{
#'   \item Filter candidates by content balancing;
#'   \item Compute adaptive criterion scores for filtered candidates;
#'   \item Select via exposure control.
#' }
#'
#' **Shadow mode** (`constr_fun` provided):
#' \enumerate{
#'   \item Compute criterion scores for all J items (full bank);
#'   \item Delegate selection to `constr_fun(scores, items, administered)`.
#' }
#'
#' @param items A `cdcat_items` object.
#' @param responses Numeric vector of 0/1/NA responses (length = n_items).
#' @param administered Integer vector of already administered item indices.
#' @param criterion A string: `"KL"`, `"PWKL"`, `"MPWKL"`, `"SHE"`,
#'   `"SEQ"`, or `"RANDOM"`.
#' @param est A list returned by [estimate_alpha()]. If `NULL`, computed
#'   internally.
#' @param method Estimation method passed to [estimate_alpha()] if
#'   `est = NULL`.
#' @param prior Prior probabilities passed to [estimate_alpha()] if
#'   `est = NULL`.
#' @param content Character vector of length J with content domain labels.
#'   `NULL` disables content balancing.
#' @param content_prop Named numeric vector of target domain proportions.
#'   `NULL` disables content balancing.
#' @param exposure Numeric vector of length J with exposure parameters.
#'   `NULL` disables exposure control.
#' @param constr_fun Constraint function for shadow mode, or `NULL`.
#' @param initial_profile Binary integer vector of length K for first-item
#'   anchor override, or `NULL`.
#' @param return_details Logical. If `TRUE`, returns a named list with
#'   selection details instead of a plain integer. Default `FALSE`.
#'
#' @return
#'   When `return_details = FALSE` (default): integer scalar -- item index.
#'
#'   When `return_details = TRUE`: named list with fields:
#'   \describe{
#'     \item{`item`}{Integer. Selected item index.}
#'     \item{`candidate_items`}{Integer vector after content filter.}
#'     \item{`criterion_scores`}{Named numeric vector of scores (names are
#'       item indices), or `NULL` for non-adaptive criteria.}
#'     \item{`item_pre_exposure`}{Integer. Item that would have been selected
#'       without exposure control, or `NA` if not applicable.}
#'     \item{`exposure_redirected`}{Logical. Whether exposure control changed
#'       the selected item.}
#'   }
#'
#' @references
#' Kingsbury, G. G., & Zara, A. R. (1991). A comparison of procedures
#' for content-sensitive item selection in computerized adaptive
#' testing. *Applied Measurement in Education*, 4(3), 241--261.
#'
#' Sympson, J. B., & Hetter, R. D. (1985). *Controlling item-exposure
#' rates in computerized adaptive testing*. Proceedings of the 27th
#' annual meeting of the Military Testing Association (pp. 973--977).
#'
#' van der Linden, W. J. (2005). *Linear models for optimal test design*.
#' Springer.
#'
#' @seealso [apply_content_balancing()], [apply_exposure_control()],
#'   [CdcatSession]
#'
#' @export
select_next_item <- function(
    items,
    responses,
    administered,
    criterion,
    est             = NULL,
    method          = "MAP",
    prior           = NULL,
    content         = NULL,
    content_prop    = NULL,
    exposure        = NULL,
    constr_fun      = NULL,
    initial_profile = NULL,
    return_details  = FALSE
) {

  criterion <- toupper(criterion)

  # ----------------------------------------------------------------
  # SHADOW MODE
  # ----------------------------------------------------------------
  if (!is.null(constr_fun)) {

    if (criterion %in% c("SEQ", "RANDOM"))
      warning(
        "Shadow CAT is active but criterion = '", criterion,
        "' does not produce meaningful scores. ",
        "Consider an adaptive criterion (PWKL, KL, MPWKL, SHE)."
      )

    if (is.null(est))
      est <- estimate_alpha(responses, items, method = method, prior = prior)

    est  <- .override_alpha_hat(est, items, administered, initial_profile)
    item <- .apply_shadow_cat(items, est, administered, criterion, constr_fun)

    if (!return_details)
      return(item)

    all_items    <- seq_len(items$n_items)
    scores_full  <- .score_items(all_items, criterion, est)
    scores_named <- setNames(scores_full, as.character(all_items))

    return(list(
      item                = item,
      candidate_items     = setdiff(all_items, administered),
      criterion_scores    = scores_named,
      item_pre_exposure   = NA_integer_,
      exposure_redirected = FALSE
    ))
  }

  # ----------------------------------------------------------------
  # GREEDY MODE
  # ----------------------------------------------------------------

  # 1. Available items
  available <- setdiff(seq_len(items$n_items), administered)
  if (length(available) == 0) {
    if (!return_details) return(0L)
    return(list(item = 0L, candidate_items = integer(0),
                criterion_scores = NULL, item_pre_exposure = NA_integer_,
                exposure_redirected = FALSE))
  }

  # 2. Content balancing filter
  candidates <- apply_content_balancing(
    candidate_items = available,
    administered    = administered,
    content         = content,
    content_prop    = content_prop
  )

  # 3. Non-adaptive criteria
  if (criterion == "SEQ") {
    item <- candidates[1]
    if (!return_details) return(item)
    return(list(item = item, candidate_items = candidates,
                criterion_scores = NULL, item_pre_exposure = NA_integer_,
                exposure_redirected = FALSE))
  }

  if (criterion == "RANDOM") {
    item <- candidates[sample(length(candidates), 1L)]
    if (!return_details) return(item)
    return(list(item = item, candidate_items = candidates,
                criterion_scores = NULL, item_pre_exposure = NA_integer_,
                exposure_redirected = FALSE))
  }

  # 4. Estimation
  if (is.null(est))
    est <- estimate_alpha(responses, items, method = method, prior = prior)

  est <- .override_alpha_hat(est, items, administered, initial_profile)

  # 5. Adaptive scores
  scores       <- .score_items(candidates, criterion, est)
  scores_named <- setNames(scores, as.character(candidates))

  # 6. Pre-exposure best item
  item_pre_exposure <- candidates[which.max(scores)]

  # 7. Exposure control -> final item
  item <- apply_exposure_control(
    scores         = scores,
    available      = candidates,
    exposure       = exposure,
    n_administered = length(administered)
  )

  exposure_redirected <- !identical(item, item_pre_exposure)

  if (!return_details) return(item)

  list(
    item                = item,
    candidate_items     = candidates,
    criterion_scores    = scores_named,
    item_pre_exposure   = item_pre_exposure,
    exposure_redirected = exposure_redirected
  )
}


# ------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------

#' Score a set of items by criterion (internal)
#'
#' @param item_indices Integer vector of item indices to score.
#' @param criterion Upper-cased criterion string.
#' @param est A `cdcat_est` object.
#' @return Numeric vector of scores (same length as `item_indices`).
#' @keywords internal
.score_items <- function(item_indices, criterion, est) {

  prob_matrix     <- est$prob_matrix
  posterior       <- est$posterior
  alpha_hat_index <- est$alpha_hat_index

  sapply(item_indices, function(j) {
    switch(
      criterion,
      KL    = KL_criteria(j, alpha_hat_index, prob_matrix),
      PWKL  = PWKL_criteria(j, alpha_hat_index, prob_matrix, posterior),
      MPWKL = MPWKL_criteria(j, prob_matrix, posterior),
      SHE   = -SHE_criteria(j, prob_matrix, posterior),
      stop(sprintf("Unknown criterion: '%s'.", criterion))
    )
  })
}


#' Override alpha_hat_index for first-item selection (internal)
#'
#' @param est A `cdcat_est` object.
#' @param items A `cdcat_items` object.
#' @param administered Integer vector of administered item indices.
#' @param initial_profile Binary integer vector of length K, or `NULL`.
#' @return The (possibly modified) `cdcat_est` object.
#' @keywords internal
.override_alpha_hat <- function(est, items, administered, initial_profile) {

  if (is.null(initial_profile) || length(administered) > 0)
    return(est)

  sp  <- est$skill_patterns
  idx <- which(apply(sp, 1, function(row) all(row == initial_profile)))

  if (length(idx) != 1L)
    stop(sprintf(
      "initial_profile [%s] does not match any skill profile for K = %d.",
      paste(initial_profile, collapse = ", "),
      items$n_attrs
    ))

  est$alpha_hat_index <- idx
  est$alpha_hat       <- sp[idx, , drop = FALSE]
  est
}


#' Select the first item in a CD-CAT session (internal)
#'
#' Handles the `start_item` logic for the first item only. Delegates to
#' [select_next_item()] when `start_item` is a criterion string.
#'
#' @param start_item Integer index, or character criterion string (upper-cased).
#' @param items A `cdcat_items` object.
#' @param criterion The session's main criterion (passed to
#'   [select_next_item()] when `start_item` is a criterion name).
#' @param method Estimation method.
#' @param prior Prior probability vector, or `NULL`.
#' @param initial_profile Binary integer vector of length K, or `NULL`.
#' @return Integer scalar -- selected item index.
#' @keywords internal
.select_first_item <- function(start_item, items, criterion, method, prior,
                               initial_profile) {

  if (is.integer(start_item))
    return(start_item)

  # Character: use it as the criterion for first-item selection
  select_next_item(
    items           = items,
    responses       = rep(NA_real_, items$n_items),
    administered    = integer(0),
    criterion       = start_item,          # already upper-cased
    method          = method,
    prior           = prior,
    initial_profile = initial_profile,
    return_details  = FALSE
  )
}


#' Validate the start_item parameter (internal)
#'
#' @param start_item Integer index, character criterion name, or `NULL`.
#' @param n_items Integer. Total number of items in the bank.
#' @return Invisibly `TRUE`. Stops on error.
#' @keywords internal
.validate_start_item <- function(start_item, n_items) {

  if (is.null(start_item))
    return(invisible(TRUE))

  if (is.numeric(start_item) || is.integer(start_item)) {
    val <- suppressWarnings(as.integer(start_item))
    if (length(val) != 1L || is.na(val) || val < 1L || val > n_items)
      stop(sprintf(
        "start_item numeric must be a positive integer in [1, %d].",
        n_items
      ))
    return(invisible(TRUE))
  }

  if (is.character(start_item)) {
    valid <- c("RANDOM", "SEQ", "KL", "PWKL", "MPWKL", "SHE")
    if (!toupper(start_item) %in% valid)
      stop(sprintf(
        "start_item character must be one of: %s.",
        paste(valid, collapse = ", ")
      ))
    return(invisible(TRUE))
  }

  stop(
    "start_item must be a positive integer or one of: ",
    "'random', 'seq', 'KL', 'PWKL', 'MPWKL', 'SHE'."
  )
}


#' Validate initial_profile (internal)
#'
#' @param initial_profile Binary integer vector of length K, or `NULL`.
#' @param K Integer. Number of attributes.
#' @return Invisibly `TRUE`. Stops on error.
#' @keywords internal
.validate_initial_profile <- function(initial_profile, K) {

  if (is.null(initial_profile))
    return(invisible(TRUE))

  if (!is.numeric(initial_profile) && !is.integer(initial_profile))
    stop("initial_profile must be a numeric or integer vector.")

  initial_profile <- as.integer(initial_profile)

  if (length(initial_profile) != K)
    stop(sprintf(
      "initial_profile must have length K = %d. Got length %d.",
      K, length(initial_profile)
    ))

  if (any(!initial_profile %in% c(0L, 1L)))
    stop("initial_profile must contain only 0s and 1s.")

  invisible(TRUE)
}
