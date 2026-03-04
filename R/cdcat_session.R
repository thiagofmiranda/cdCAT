#' @import R6
#' @importFrom stats setNames
NULL

#' CD-CAT Session Object
#'
#' An R6 class implementing the adaptive testing cycle for a single
#' cognitive diagnosis computerized adaptive testing (CD-CAT) session.
#'
#' @details
#' This class orchestrates the sequential CD-CAT procedure:
#'
#' \enumerate{
#'   \item Initialize the posterior distribution over latent profiles;
#'   \item Select the next item according to a specified selection criterion;
#'   \item Observe the response and update the posterior;
#'   \item Evaluate stopping criteria;
#'   \item Repeat until termination.
#' }
#'
#' The adaptive algorithm assumes:
#' \itemize{
#'   \item Binary item responses;
#'   \item Conditional independence of item responses given the latent profile;
#'   \item A calibrated item bank defined by a Q-matrix and fixed item parameters;
#'   \item A specified estimation method (MLE, MAP, or EAP);
#'   \item A specified item selection criterion (e.g., KL, PWKL, MPWKL, SHE).
#' }
#'
#' The session object maintains the full state of the adaptive test,
#' including responses, administered items, posterior distribution,
#' and stopping status.
#'
#' **Implementation details**
#'
#' Each call to `next_item()` evaluates stopping criteria, computes item
#' selection (with full diagnostic details), and caches the result in
#' `private$pending`. Each call to `update(item, response)` records the
#' response, re-estimates the attribute profile, and appends a complete
#' step record to `self$history`.
#'
#' **Per-step history**
#'
#' The `history` field stores one named list per administered item.
#' Use `session$history_df()` to convert to a flat data.frame.
#' Each record contains:
#'
#' \describe{
#'   \item{`step`}{Integer. 1-based position in the test.}
#'   \item{`item`}{Integer. Item index administered.}
#'   \item{`response`}{Integer. 0 or 1.}
#'   \item{`candidate_items`}{Integer vector. Items available after
#'         content balancing filter.}
#'   \item{`criterion_scores`}{Named numeric vector. Criterion score per
#'         candidate item (`NULL` for SEQ/RANDOM).}
#'   \item{`item_pre_exposure`}{Integer. Item that would have been chosen
#'         without exposure control (`NA` in shadow/non-adaptive mode).}
#'   \item{`exposure_redirected`}{Logical. Whether exposure control
#'         redirected the selection.}
#'   \item{`target_domain`}{Character. Content domain targeted by
#'         balancing at this step (`NULL` if inactive).}
#'   \item{`domain_gap`}{Named numeric. Gap (target - observed) per
#'         domain at this step (`NULL` if inactive).}
#'   \item{`stop_check`}{List with `stop` (logical) and `reason`
#'         (character) from the stopping evaluation before this item.}
#'   \item{`posterior`}{Numeric vector of length 2^K. Posterior after
#'         this response.}
#'   \item{`alpha_hat`}{Integer vector of length K. Estimated profile.}
#'   \item{`alpha_hat_index`}{Integer. Row index of `alpha_hat` in the
#'         skill patterns matrix.}
#'   \item{`mastery_marginal`}{Named numeric of length K.
#'         P(alpha_k = 1) for each attribute after this response.}
#'   \item{`timestamp`}{POSIXct. Time at which `update()` was called.}
#'   \item{`response_time`}{Numeric. Seconds elapsed since the previous
#'         `update()` call (or session start for the first item).}
#' }
#'
#' @references
#' Rupp, A. A., Templin, J., & Henson, R. (2010).
#' Diagnostic Measurement: Theory, Methods, and Applications.
#'
#' @export
CdcatSession <- R6::R6Class(
  classname = "CdcatSession",

  # ----------------------------------------------------------------
  # Private state
  # ----------------------------------------------------------------
  private = list(
    # Holds selection details from next_item() until update() picks them up
    pending   = NULL,
    # POSIXct of the last update() call (or initialization)
    last_time = NULL
  ),

  public = list(

    # -- Configuration fields --
    #' @field items A `cdcat_items` object.
    items = NULL,
    #' @field method Estimation method (`"MAP"`, `"MLE"`, or `"EAP"`).
    method = NULL,
    #' @field criterion Item selection criterion.
    criterion = NULL,
    #' @field min_items Minimum items before stopping is allowed.
    min_items = NULL,
    #' @field max_items Maximum items allowed.
    max_items = NULL,
    #' @field threshold Posterior threshold for attribute classification.
    threshold = NULL,
    #' @field prior Prior probability vector over skill profiles, or `NULL`.
    prior = NULL,
    #' @field content Character vector of domain labels per item, or `NULL`.
    content = NULL,
    #' @field content_prop Named numeric vector of target domain proportions.
    content_prop = NULL,
    #' @field exposure Numeric exposure vector per item, or `NULL`.
    exposure = NULL,
    #' @field constr_fun Shadow CAT constraint function, or `NULL`.
    constr_fun = NULL,
    #' @field initial_profile Binary integer vector of length K for
    #'   first-item anchor override, or `NULL`.
    initial_profile = NULL,
    #' @field start_item Integer or character. Method for selecting the
    #'   first item.
    start_item = NULL,

    # -- Session state fields --
    #' @field responses Numeric vector of responses (NA = not administered).
    responses = NULL,
    #' @field administered Integer vector of administered item indices.
    administered = NULL,
    #' @field est Latest `cdcat_est` estimation result.
    est = NULL,
    #' @field stop_reason Character stopping reason, or `NULL`.
    stop_reason = NULL,
    #' @field history List of per-step records (one element per administered
    #'   item). See Details for field descriptions.
    history = NULL,

    # ----------------------------------------------------------------
    #' @description Create a new CD-CAT session.
    #' @param items A `cdcat_items` object.
    #' @param method Estimation method. Default `"MAP"`.
    #' @param criterion Item selection criterion. Default `"PWKL"`.
    #' @param min_items Minimum items. Default `1`.
    #' @param max_items Maximum items. Default `20`.
    #' @param threshold Classification threshold. Default `0.8`.
    #' @param prior Prior probability vector (length 2^K) or output of
    #'   [cdcat_prior()]. `NULL` uses uniform prior.
    #' @param content Character vector of length J with domain labels.
    #'   `NULL` disables content balancing.
    #' @param content_prop Named numeric vector of target domain proportions.
    #'   `NULL` disables content balancing.
    #' @param exposure Numeric vector of length J. Values in `[0,1]` ->
    #'   Sympson-Hetter; values `>= 1` -> Randomesque. `NULL` disables.
    #' @param constr_fun Constraint function for shadow CAT, or `NULL`.
    #' @param initial_profile Binary integer vector of length K. When
    #'   provided, used as the anchor profile for KL/PWKL on the first
    #'   item selection only; does not affect the posterior.
    #' @param start_item Integer or character. Specifies how the first item
    #'   is selected. An integer administers that specific item index; `"random"`
    #'   selects randomly; `"seq"` selects the first available item; a criterion
    #'   name (`"KL"`, `"PWKL"`, `"MPWKL"`, `"SHE"`) applies that criterion
    #'   for the first selection only. `NULL` (default) applies the main
    #'   `criterion` from the very first item, including content balancing,
    #'   exposure control, and shadow CAT constraints.
    initialize = function(
    items,
    method          = "MAP",
    criterion       = "PWKL",
    min_items       = 1L,
    max_items       = 20L,
    threshold       = 0.8,
    prior           = NULL,
    content         = NULL,
    content_prop    = NULL,
    exposure        = NULL,
    constr_fun      = NULL,
    initial_profile = NULL,
    start_item      = NULL
    ) {
      if (!inherits(items, "cdcat_items"))
        stop("items must be a cdcat_items object.")

      .validate_prior(prior, items$n_attrs)
      .validate_content(content, content_prop, items$n_items)
      .validate_exposure(exposure, items$n_items)
      .validate_constr_fun(constr_fun)
      .validate_initial_profile(initial_profile, items$n_attrs)
      .validate_start_item(start_item, items$n_items)

      if (!is.null(constr_fun) &&
          (!is.null(content) || !is.null(exposure)))
        warning(
          "Shadow CAT (constr_fun) is active. ",
          "content_prop and exposure will be ignored -- ",
          "enforce all constraints inside constr_fun."
        )

      self$items           <- items
      self$method          <- method
      self$criterion       <- criterion
      self$min_items       <- as.integer(min_items)
      self$max_items       <- as.integer(max_items)
      self$threshold       <- threshold
      self$prior           <- .validate_prior(prior, items$n_attrs)
      self$content         <- content
      self$content_prop    <- content_prop
      self$exposure        <- exposure
      self$constr_fun      <- constr_fun
      self$initial_profile <- if (!is.null(initial_profile))
        as.integer(initial_profile)
      else NULL
      self$start_item      <- if (is.null(start_item))
        NULL
      else if (is.numeric(start_item) || is.integer(start_item))
        as.integer(start_item)
      else
        toupper(start_item)

      self$responses       <- rep(NA_real_, items$n_items)
      self$administered    <- integer(0)
      self$stop_reason     <- NULL
      self$history         <- list()

      self$est <- estimate_alpha(
        self$responses, self$items,
        method = self$method, prior = self$prior
      )

      private$last_time <- Sys.time()
      private$pending   <- NULL
    },

    # ----------------------------------------------------------------
    #' @description Select the next item to administer.
    #'
    #' Evaluates stopping criteria, computes item selection, and caches
    #' selection details internally for `update()` to incorporate into
    #' the history record.
    #'
    #' @return Integer index of the next item, or `0L` if stopped.
    next_item = function() {

      if (!is.null(self$stop_reason))
        return(0L)

      # -- First item: delegate to start_item logic (only when explicitly set)
      if (length(self$administered) == 0 && !is.null(self$start_item)) {
        first_item <- .select_first_item(
          start_item      = self$start_item,
          items           = self$items,
          criterion       = self$criterion,
          method          = self$method,
          prior           = self$prior,
          initial_profile = self$initial_profile
        )

        private$pending <- list(
          item                = first_item,
          candidate_items     = seq_len(self$items$n_items),
          criterion_scores    = NULL,
          item_pre_exposure   = NA_integer_,
          exposure_redirected = FALSE,
          target_domain       = NULL,
          domain_gap          = NULL,
          stop_check          = list(stop = FALSE, reason = NA_character_)
        )

        return(first_item)
      }

      # -- Stopping check
      stop_check <- check_stopping(
        est            = self$est,
        n_administered = length(self$administered),
        min_items      = self$min_items,
        max_items      = self$max_items,
        threshold      = self$threshold
      )

      if (stop_check$stop) {
        self$stop_reason <- stop_check$reason
        return(0L)
      }

      # -- Domain gap (before filter, reflects current state)
      domain_gap <- .compute_domain_gap(
        administered = self$administered,
        content      = self$content,
        content_prop = self$content_prop
      )

      target_domain <- if (!is.null(domain_gap))
        names(which.max(domain_gap))
      else
        NULL

      # -- Item selection (with full details)
      sel <- select_next_item(
        items           = self$items,
        responses       = self$responses,
        administered    = self$administered,
        criterion       = self$criterion,
        est             = self$est,
        method          = self$method,
        prior           = self$prior,
        content         = self$content,
        content_prop    = self$content_prop,
        exposure        = self$exposure,
        constr_fun      = self$constr_fun,
        initial_profile = self$initial_profile,
        return_details  = TRUE
      )

      # -- Cache pending selection details for update()
      private$pending <- list(
        item                = sel$item,
        candidate_items     = sel$candidate_items,
        criterion_scores    = sel$criterion_scores,
        item_pre_exposure   = sel$item_pre_exposure,
        exposure_redirected = sel$exposure_redirected,
        target_domain       = target_domain,
        domain_gap          = domain_gap,
        stop_check          = stop_check
      )

      sel$item
    },

    # ----------------------------------------------------------------
    #' @description Register a response and update session state.
    #'
    #' Records the response, re-estimates the attribute profile, and
    #' appends a complete step record to `self$history`.
    #'
    #' @param item Integer. Index of the administered item.
    #' @param response Numeric. 1 (correct) or 0 (incorrect).
    update = function(item, response) {

      if (!item %in% seq_len(self$items$n_items))
        stop(sprintf("item index %d is out of range.", item))

      if (item %in% self$administered)
        stop(sprintf("Item %d has already been administered.", item))

      if (!response %in% c(0, 1))
        stop("response must be 0 or 1.")

      # -- Record response
      self$responses[item] <- response
      self$administered    <- c(self$administered, as.integer(item))

      # -- Re-estimate
      self$est <- estimate_alpha(
        self$responses, self$items,
        method = self$method, prior = self$prior
      )

      # -- Timing
      now           <- Sys.time()
      response_time <- as.numeric(difftime(now, private$last_time,
                                           units = "secs"))
      private$last_time <- now

      # -- Pending details (from last next_item() call)
      # If update() is called without a prior next_item() (manual use),
      # build a minimal pending record.
      pending <- if (!is.null(private$pending) &&
                     identical(private$pending$item, as.integer(item))) {
        private$pending
      } else {
        list(
          item                = as.integer(item),
          candidate_items     = integer(0),
          criterion_scores    = NULL,
          item_pre_exposure   = NA_integer_,
          exposure_redirected = FALSE,
          target_domain       = NULL,
          domain_gap          = NULL,
          stop_check          = list(stop = FALSE, reason = "manual")
        )
      }
      private$pending <- NULL

      # -- Build and append history step
      step_record <- .build_history_step(
        step          = length(self$administered),
        item          = as.integer(item),
        response      = as.integer(response),
        pending       = pending,
        est           = self$est,
        timestamp     = now,
        response_time = response_time
      )
      self$history <- c(self$history, list(step_record))

      invisible(self)
    },

    # ----------------------------------------------------------------
    #' @description Convert the history list to a flat data.frame.
    #'
    #' Vector-valued fields (posterior, mastery_marginal, alpha_hat,
    #' criterion_scores, candidate_items, domain_gap) are kept as
    #' list-columns.
    #'
    #' @return A data.frame with one row per administered item, or an
    #'   empty data.frame if no items have been administered.
    history_df = function() {
      .history_to_df(self$history)
    },

    # ----------------------------------------------------------------
    #' @description Extract final session results.
    #' @return Named list with estimation results and session metadata.
    result = function() {
      list(
        alpha_hat    = self$est$alpha_hat,
        posterior    = self$est$posterior,
        administered = self$administered,
        responses    = self$responses[self$administered],
        n_items      = length(self$administered),
        stop_reason  = self$stop_reason
      )
    },

    # ----------------------------------------------------------------
    #' @description Print a summary of the session state.
    #' @param ... Ignored.
    print = function(...) {
      n  <- length(self$administered)
      cat("CdcatSession\n")
      cat("  Model    :", self$items$model, "\n")
      cat("  Method   :", self$method, "\n")
      cat("  Criterion:", self$criterion, "\n")
      if (!is.null(self$start_item)) {
        if (is.integer(self$start_item)) {
          cat(sprintf("  Start    : item %d\n", self$start_item))
        } else {
          cat(sprintf("  Start    : %s\n", self$start_item))
        }
      }
      cat("  Items    :", n, "/", self$max_items, "administered\n")
      cat("  Prior    :", if (is.null(self$prior)) "uniform" else "custom", "\n")
      cat("  Init.prof:", if (is.null(self$initial_profile)) "none"
          else paste(self$initial_profile, collapse = ""), "\n")
      cat("  Content  :", if (is.null(self$content)) "none" else "active", "\n")
      cat("  Exposure :", if (is.null(self$exposure)) "none"
          else if (all(self$exposure <= 1)) "Sympson-Hetter"
          else "Randomesque", "\n")
      cat("  Shadow   :", if (is.null(self$constr_fun)) "no" else "yes", "\n")
      cat("  History  :", n, "step(s) recorded\n")
      if (!is.null(self$stop_reason))
        cat("  Stopped  :", self$stop_reason, "\n")
      invisible(self)
    }
  )
)
