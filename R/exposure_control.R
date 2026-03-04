# exposure_control.R -- Controle de exposicao de itens para CD-CAT
#
# Funcoes exportadas : apply_exposure_control()
#                      apply_sympson_hetter()
#                      apply_randomesque()
# Funcoes internas   : .validate_exposure()


# ------------------------------------------------------------------
# Funcao publica principal: despachante de regime
# ------------------------------------------------------------------

#' Apply exposure control to item selection
#'
#' Selects the next item from the candidate pool according to the
#' exposure control regime encoded in `exposure`.
#'
#' Two regimes are supported and detected automatically from the values
#' of `exposure`:
#'
#' **Sympson-Hetter** (Sympson & Hetter, 1985) -- triggered when **all**
#' values in `exposure` are in \eqn{[0, 1]}.  Each item has an
#' acceptance probability equal to its `exposure` value.  Items are
#' visited in descending score order; the first one that passes the
#' random draw is selected.  Values near 1 are almost always selected;
#' values near 0 are rarely selected.
#'
#' **Randomesque** -- triggered when **all** values in `exposure` are
#' \eqn{\geq 1}.  At position \eqn{k} in the test (i.e. when selecting
#' the \eqn{k}-th item), the top `exposure[k]` candidates by score are
#' collected and one is drawn at random.  `exposure[k] = 1` is
#' equivalent to greedy selection.
#'
#' When `exposure` is `NULL`, greedy selection (`which.max(scores)`) is
#' used, equivalent to no exposure control.
#'
#' @param scores Numeric vector of criterion scores for the candidate
#'   items (same length and order as `available`).
#' @param available Integer vector of candidate item indices (items
#'   eligible for selection at this step).
#' @param exposure Numeric vector of length J (total items) with
#'   exposure parameters, or `NULL` for no control.  See Details.
#' @param n_administered Integer. Number of items already administered
#'   (used to determine position \eqn{k} in the Randomesque regime).
#'
#' @return Integer scalar -- global index of the selected item.
#'
#' @references
#' Sympson, J. B., & Hetter, R. D. (1985). *Controlling item-exposure
#' rates in computerized adaptive testing*. Proceedings of the 27th
#' annual meeting of the Military Testing Association (pp. 973--977).
#'
#' @seealso [apply_sympson_hetter()], [apply_randomesque()],
#'   [select_next_item()], [CdcatSession]
#'
#' @export
#'
#' @examples
#' scores    <- c(0.8, 0.5, 0.3, 0.9)
#' available <- c(2L, 4L, 6L, 8L)
#'
#' # Greedy (no exposure control)
#' apply_exposure_control(scores, available, exposure = NULL, n_administered = 0L)
#'
#' # Sympson-Hetter: item 8 (score 0.9) has probability 0.2 of being accepted
#' set.seed(1)
#' exposure_sh <- rep(0.8, 10)
#' exposure_sh[8] <- 0.2
#' apply_exposure_control(scores, available, exposure_sh, n_administered = 2L)
#'
#' # Randomesque: at position 3, draw from top-2 candidates
#' exposure_rq <- rep(1, 10)
#' exposure_rq[3] <- 2
#' set.seed(1)
#' apply_exposure_control(scores, available, exposure_rq, n_administered = 2L)
apply_exposure_control <- function(scores, available, exposure, n_administered) {

  # --- No control: greedy
  if (is.null(exposure))
    return(available[which.max(scores)])

  # --- Sympson-Hetter regime: all values in [0, 1]
  if (all(exposure >= 0 & exposure <= 1)) {
    return(apply_sympson_hetter(scores, available, exposure))
  }

  # --- Randomesque regime: all values >= 1
  if (all(exposure >= 1)) {
    k <- n_administered + 1L

    # Fallback: se já administramos mais itens que o comprimento de exposure
    if (k > length(exposure)) {
      return(available[which.max(scores)])
    }

    # Determinar n (número de top candidatos)
    n <- if (!is.na(exposure[k])) {
      as.integer(exposure[k])
    } else {
      1L
    }

    return(apply_randomesque(scores, available, n))
  }

  stop(
    "Invalid exposure vector: all values must be in [0, 1] (Sympson-Hetter) ",
    "or all >= 1 (Randomesque). Mixed values are not allowed."
  )
}


# ------------------------------------------------------------------
# Sympson-Hetter
# ------------------------------------------------------------------

#' Sympson-Hetter exposure control
#'
#' Selects an item by iterating over candidates in descending score
#' order and accepting the first one that passes a Bernoulli draw
#' with probability `p[item]` (Sympson & Hetter, 1985).
#'
#' @param scores Numeric vector of scores for the candidate items.
#' @param available Integer vector of global item indices corresponding
#'   to `scores`.
#' @param p Numeric vector of acceptance probabilities in [0, 1], one per item in the bank (length J),
#'   The function extracts probabilities for available items internally.
#'
#' @return Integer scalar -- global index of the selected item.
#'
#' @references
#' Sympson, J. B., & Hetter, R. D. (1985). *Controlling item-exposure
#' rates in computerized adaptive testing*. Proceedings of the 27th
#' annual meeting of the Military Testing Association (pp. 973--977).
#'
#' @seealso [apply_exposure_control()]
#' @export
#' @importFrom stats runif
apply_sympson_hetter <- function(scores, available, p) {

  ord <- order(scores, decreasing = TRUE)

  for (r in ord) {

    item_id <- available[r]
    prob    <- p[item_id]

    if (is.na(prob))
      stop("Exposure probability is NA for item ", item_id)

    if (runif(1L) <= prob)
      return(item_id)
  }

  warning(
    "Sympson-Hetter: all candidate items were rejected in this draw. ",
    "Falling back to the highest-scoring item."
  )

  available[which.max(scores)]
}


# ------------------------------------------------------------------
# Randomesque
# ------------------------------------------------------------------

#' Randomesque exposure control
#'
#' Selects an item by drawing uniformly at random from the top-`n`
#' candidates by score.  When `n = 1` the result is identical to
#' greedy (deterministic) selection.
#'
#' @param scores Numeric vector of scores for the candidate items.
#' @param available Integer vector of global item indices corresponding
#'   to `scores`.
#' @param n Integer. Number of top-scoring candidates to include in the
#'   random draw.  Capped at `length(scores)` if larger.
#'
#' @return Integer scalar -- global index of the selected item.
#'
#' @seealso [apply_exposure_control()]
#' @export
apply_randomesque <- function(scores, available, n) {

  n <- min(as.integer(n), length(scores))

  if (n <= 1L)
    return(available[which.max(scores)])

  top_idx <- order(scores, decreasing = TRUE)[seq_len(n)]
  available[sample(top_idx, 1L)]
}


# ------------------------------------------------------------------
# Validacao interna
# ------------------------------------------------------------------

#' Validate exposure control vector (internal)
#'
#' Called by [CdcatSession] at initialization to detect
#' misconfigured exposure vectors before the session starts.
#'
#' @param exposure Numeric vector of length J, or `NULL`.
#' @param n_items Integer. Total items in the bank (J).
#'
#' @return Invisibly `TRUE`. Stops with an informative message on error.
#' @keywords internal
.validate_exposure <- function(exposure, n_items) {

  if (is.null(exposure))
    return(invisible(TRUE))

  if (!is.numeric(exposure))
    stop("exposure must be a numeric vector.")

  if (length(exposure) != n_items)
    stop(sprintf(
      "exposure must have length equal to the number of items (%d). Got %d.",
      n_items, length(exposure)
    ))

  if (any(is.na(exposure)))
    stop("exposure contains NA values.")

  if (any(exposure < 0))
    stop("exposure must contain non-negative values.")

  sh_regime  <- all(exposure >= 0 & exposure <= 1)
  rq_regime  <- all(exposure >= 1)

  if (!sh_regime && !rq_regime)
    stop(
      "exposure values must be either all in [0, 1] (Sympson-Hetter) ",
      "or all >= 1 (Randomesque). Mixed values are not allowed."
    )

  invisible(TRUE)
}
