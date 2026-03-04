# content_balancing.R -- Balanceamento de conteudo para CD-CAT
#
# Funcoes exportadas : apply_content_balancing()
# Funcoes internas   : .validate_content()


#' Apply content balancing to candidate items
#'
#' Filters the candidate item pool so that the next item is drawn from
#' the content domain most under-represented relative to the target
#' blueprint (Kingsbury & Zara, 1991).
#'
#' The gap for each domain is defined as:
#' \deqn{\text{gap}_d = \text{prop\_target}_d - \text{prop\_observed}_d}
#' The domain with the largest gap is selected, and only items from
#' that domain are returned.  If no candidate items belong to the
#' target domain, the full candidate pool is returned as a safe
#' fallback (no items are left un-selectable).
#'
#' @param candidate_items Integer vector of indices of items eligible
#'   for selection (i.e. not yet administered).
#' @param administered Integer vector of indices of items already
#'   administered.  Length 0 is valid (no items administered yet).
#' @param content Character vector of length J (total item bank size)
#'   giving the content domain label for each item.
#'   Example: `c("algebra", "algebra", "geometry", "geometry")`.
#' @param content_prop Named numeric vector of target proportions for
#'   each domain.  Names must match the unique values in `content`.
#'   Values must be non-negative and sum to 1.
#'   Example: `c(algebra = 0.6, geometry = 0.4)`.
#'
#' @return Integer vector of candidate item indices, restricted to the
#'   most under-represented domain (or the full `candidate_items` if
#'   no balancing is needed or no candidates belong to that domain).
#'
#' @references
#' Kingsbury, G. G., & Zara, A. R. (1991). A comparison of procedures
#' for content-sensitive item selection in computerized adaptive
#' testing. *Applied Measurement in Education*, 4(3), 241--261.
#'
#' @seealso [select_next_item()], [CdcatSession]
#'
#' @export
#'
#' @examples
#' content      <- c("A", "A", "A", "B", "B", "B")
#' content_prop <- c(A = 0.5, B = 0.5)
#'
#' # No items administered yet -- returns all candidates unchanged
#' apply_content_balancing(1:6, integer(0), content, content_prop)
#'
#' # After two "A" items: gap favours "B"
#' apply_content_balancing(3:6, c(1L, 2L), content, content_prop)
#' @importFrom stats setNames
apply_content_balancing <- function(
    candidate_items,
    administered,
    content,
    content_prop
) {

  # --- Passthrough when balancing is not configured
  if (is.null(content) || is.null(content_prop))
    return(candidate_items)

  # --- No items administered yet: nothing to balance against
  if (length(administered) == 0)
    return(candidate_items)

  # --- Observed proportion: build a clean named numeric vector
  #     (avoids table arithmetic misalignment)
  observed_prop <- setNames(numeric(length(content_prop)), names(content_prop))
  domain_counts <- table(content[administered])
  common        <- intersect(names(domain_counts), names(observed_prop))
  observed_prop[common] <- as.numeric(domain_counts[common]) / length(administered)

  # --- Gap (Kingsbury & Zara, 1991)
  gap           <- as.numeric(content_prop) - observed_prop
  names(gap)    <- names(content_prop)
  target_domain <- names(gap)[which.max(gap)]

  # --- Filter candidates to target domain
  filtered <- candidate_items[content[candidate_items] == target_domain]

  # Safe fallback: if no candidates belong to the target domain,
  # return the full candidate pool so the test can continue.
  if (length(filtered) == 0)
    return(candidate_items)

  filtered
}


#' Validate content balancing parameters (internal)
#'
#' Called by [CdcatSession] during initialization to catch
#' misconfigured content balancing before the session starts.
#'
#' @param content Character vector of length J.
#' @param content_prop Named numeric vector.
#' @param n_items Integer. Total number of items in the bank (J).
#'
#' @return Invisibly `TRUE`. Stops with an informative message on error.
#' @keywords internal
.validate_content <- function(content, content_prop, n_items) {

  # Both NULL -> no balancing, always valid
  if (is.null(content) && is.null(content_prop))
    return(invisible(TRUE))

  # Must be supplied together
  if (is.null(content))
    stop("content_prop supplied but content is NULL. Both must be provided together.")

  if (is.null(content_prop))
    stop("content supplied but content_prop is NULL. Both must be provided together.")

  # content: type and length
  if (!is.character(content))
    stop("content must be a character vector.")

  if (length(content) != n_items)
    stop(sprintf(
      "content must have length equal to the number of items (%d). Got %d.",
      n_items, length(content)
    ))

  if (any(is.na(content)))
    stop("content contains NA values.")

  # content_prop: named numeric
  if (!is.numeric(content_prop))
    stop("content_prop must be a named numeric vector.")

  if (is.null(names(content_prop)) || any(nchar(names(content_prop)) == 0))
    stop("content_prop must be a named vector (domain names as names).")

  if (any(is.na(content_prop)))
    stop("content_prop contains NA values.")

  if (any(content_prop < 0))
    stop("content_prop must contain non-negative values.")

  if (abs(sum(content_prop) - 1) > 1e-8)
    stop(sprintf(
      "content_prop must sum to 1. Got %.6g.",
      sum(content_prop)
    ))

  # All domains in content_prop must appear in content
  missing_domains <- setdiff(names(content_prop), unique(content))
  if (length(missing_domains) > 0)
    stop(paste(
      "content_prop references domains not found in content:",
      paste(missing_domains, collapse = ", ")
    ))

  # All domains in content must be covered by content_prop
  extra_domains <- setdiff(unique(content), names(content_prop))
  if (length(extra_domains) > 0)
    stop(paste(
      "content contains domains not listed in content_prop:",
      paste(extra_domains, collapse = ", ")
    ))

  invisible(TRUE)
}
