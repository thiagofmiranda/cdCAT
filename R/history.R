# history.R -- Per-step history tracking helpers for CdcatSession
#
# Internal functions only -- nothing exported.


# ------------------------------------------------------------------
# Marginal mastery probability for each attribute
# ------------------------------------------------------------------

#' Compute marginal mastery probability per attribute (internal)
#'
#' P(alpha_k = 1) = sum of posterior over all profiles where attribute k = 1.
#'
#' @param posterior Numeric vector of length 2^K.
#' @param skill_patterns Matrix 2^K x K of binary skill profiles.
#' @return Named numeric vector of length K.
#' @keywords internal
.compute_mastery_marginal <- function(posterior, skill_patterns) {
  K <- ncol(skill_patterns)
  vals <- sapply(seq_len(K), function(k) {
    sum(posterior[skill_patterns[, k] == 1])
  })
  setNames(vals, paste0("A", seq_len(K)))
}


# ------------------------------------------------------------------
# Content balancing diagnostics
# ------------------------------------------------------------------

#' Compute domain gap vector (internal)
#'
#' Returns target - observed proportion for each content domain.
#' Returns NULL if content balancing is not configured or no items
#' have been administered yet.
#'
#' @param administered Integer vector of administered item indices.
#' @param content Character vector of length J with domain labels.
#' @param content_prop Named numeric vector of target proportions.
#' @return Named numeric vector (gap per domain) or NULL.
#' @keywords internal
.compute_domain_gap <- function(administered, content, content_prop) {

  if (is.null(content) || is.null(content_prop))
    return(NULL)

  if (length(administered) == 0)
    return(setNames(as.numeric(content_prop) - 0,
                    names(content_prop)))

  observed_prop <- setNames(numeric(length(content_prop)), names(content_prop))
  domain_counts <- table(content[administered])
  common        <- intersect(names(domain_counts), names(observed_prop))
  observed_prop[common] <- as.numeric(domain_counts[common]) / length(administered)

  gap <- as.numeric(content_prop) - observed_prop
  setNames(gap, names(content_prop))
}


# ------------------------------------------------------------------
# History record builder
# ------------------------------------------------------------------

#' Build a single history step record (internal)
#'
#' Called by `CdcatSession$update()` to assemble the per-step record
#' from pending selection data and the updated estimation result.
#'
#' @param step Integer. Step number (1-based).
#' @param item Integer. Item index administered.
#' @param response Integer. Response (0 or 1).
#' @param pending List of selection details from `next_item()`.
#' @param est Updated `cdcat_est` object from `estimate_alpha()`.
#' @param timestamp POSIXct. Time of `update()` call.
#' @param response_time Numeric. Seconds since previous update (or start).
#' @return A named list representing one history step.
#' @keywords internal
.build_history_step <- function(
    step,
    item,
    response,
    pending,
    est,
    timestamp,
    response_time
) {

  mastery <- .compute_mastery_marginal(est$posterior, est$skill_patterns)

  list(
    # -- Identification
    step                = step,
    item                = item,
    response            = response,

    # -- Selection details
    candidate_items     = pending$candidate_items,
    criterion_scores    = pending$criterion_scores,
    item_pre_exposure   = pending$item_pre_exposure,
    exposure_redirected = pending$exposure_redirected,

    # -- Content balancing
    target_domain       = pending$target_domain,
    domain_gap          = pending$domain_gap,

    # -- Stopping check (evaluated before this item was selected)
    stop_check          = pending$stop_check,

    # -- Estimation after response
    posterior           = est$posterior,
    alpha_hat           = as.integer(est$alpha_hat),
    alpha_hat_index     = est$alpha_hat_index,
    mastery_marginal    = mastery,

    # -- Timing
    timestamp           = timestamp,
    response_time       = response_time
  )
}


# ------------------------------------------------------------------
# History -> data.frame converter
# ------------------------------------------------------------------

#' Convert session history to a flat data.frame (internal)
#'
#' List columns (posterior, criterion_scores, candidate_items, domain_gap,
#' mastery_marginal, alpha_hat) are kept as list-columns so the data.frame
#' stays rectangular. Scalar fields are expanded normally.
#'
#' @param history List of step records produced by [.build_history_step()].
#' @return A data.frame with one row per step.
#' @keywords internal
.history_to_df <- function(history) {

  if (length(history) == 0)
    return(data.frame())

  # Scalar fields -- can be unlisted directly
  scalar_fields <- c(
    "step", "item", "response",
    "item_pre_exposure", "exposure_redirected",
    "target_domain",
    "alpha_hat_index",
    "timestamp", "response_time"
  )

  df <- as.data.frame(
    setNames(
      lapply(scalar_fields, function(f) {

        vals <- lapply(history, function(h) {
          v <- h[[f]]
          if (is.null(v)) NA else v
        })

        # Preserve POSIXct explicitly
        if (inherits(vals[[1]], "POSIXct")) {
          do.call(c, vals)
        } else {
          unlist(vals)
        }
      }),
      scalar_fields
    ),
    stringsAsFactors = FALSE
  )
  names(df) <- scalar_fields

  # stop_check -- two sub-fields
  df$stop_check_stop   <- sapply(history, function(h) h$stop_check$stop)
  df$stop_check_reason <- sapply(history, function(h) h$stop_check$reason)

  # List-columns for vector fields
  df$posterior         <- lapply(history, function(h) h$posterior)
  df$alpha_hat         <- lapply(history, function(h) h$alpha_hat)
  df$mastery_marginal  <- lapply(history, function(h) h$mastery_marginal)
  df$criterion_scores  <- lapply(history, function(h) h$criterion_scores)
  df$candidate_items   <- lapply(history, function(h) h$candidate_items)
  df$domain_gap        <- lapply(history, function(h) h$domain_gap)

  df
}
