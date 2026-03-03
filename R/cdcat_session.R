#' @import R6
NULL

#' CD-CAT Session Object
#'
#' An R6 class that manages the state of a single CD-CAT application.
#'
#' @export
CdcatSession <- R6::R6Class(
  classname = "CdcatSession",

  public = list(

    #' @field items A `cdcat_items` object.
    items = NULL,

    #' @field method Estimation method (`"MAP"`, `"MLE"`, or `"EAP"`).
    method = NULL,

    #' @field criterion Item selection criterion.
    criterion = NULL,

    #' @field min_items Minimum number of items before stopping.
    min_items = NULL,

    #' @field max_items Maximum number of items allowed.
    max_items = NULL,

    #' @field threshold Posterior threshold for attribute classification.
    threshold = NULL,

    #' @field prior Prior probability vector over skill profiles.
    prior = NULL,

    #' @field responses Numeric vector of responses (NA = not yet administered).
    responses = NULL,

    #' @field administered Integer vector of administered item indices.
    administered = NULL,

    #' @field est Latest estimation result (`cdcat_est` object).
    est = NULL,

    #' @field stop_reason Character string with the stopping reason, or NULL.
    stop_reason = NULL,

    #' @description Create a new CD-CAT session.
    #' @param items A `cdcat_items` object.
    #' @param method Estimation method. Default `"MAP"`.
    #' @param criterion Item selection criterion. Default `"PWKL"`.
    #' @param min_items Minimum items. Default `1`.
    #' @param max_items Maximum items. Default `20`.
    #' @param threshold Classification threshold. Default `0.8`.
    #' @param prior Prior vector. If `NULL`, uniform prior is used.
    initialize = function(
    items,
    method    = "MAP",
    criterion = "PWKL",
    min_items = 1L,
    max_items = 20L,
    threshold = 0.8,
    prior     = NULL
    ) {
      if (!inherits(items, "cdcat_items"))
        stop("items must be a cdcat_items object.")

      self$items        <- items
      self$method       <- method
      self$criterion    <- criterion
      self$min_items    <- as.integer(min_items)
      self$max_items    <- as.integer(max_items)
      self$threshold    <- threshold
      self$prior        <- prior
      self$responses    <- rep(NA_real_, items$n_items)
      self$administered <- integer(0)
      self$stop_reason  <- NULL

      # Estimação inicial com prior uniforme
      self$est <- estimate_alpha(
        self$responses, self$items,
        method = self$method, prior = self$prior
      )
    },

    #' @description Select the next item to administer.
    #' @return Integer index of the next item, or `0` if the test should stop.
    next_item = function() {

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

      select_next_item(
        items        = self$items,
        responses    = self$responses,
        administered = self$administered,
        criterion    = self$criterion,
        est          = self$est,
        method       = self$method,
        prior        = self$prior
      )
    },

    #' @description Register a response and update the session state.
    #' @param item_index Integer. Index of the item that was administered.
    #' @param response Numeric. Response value (0 or 1).
    update = function(item_index, response) {

      if (!item_index %in% seq_len(self$items$n_items))
        stop("item_index out of range.")

      if (item_index %in% self$administered)
        stop("Item already administered.")

      if (!response %in% c(0, 1))
        stop("response must be 0 or 1.")

      self$responses[item_index]  <- response
      self$administered           <- c(self$administered, item_index)

      self$est <- estimate_alpha(
        self$responses, self$items,
        method = self$method, prior = self$prior
      )

      invisible(self)
    },

    #' @description Return the final result of the session.
    #' @return A list with the estimated profile, posterior, and history.
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

    #' @description Print a summary of the current session state.
    #' @param ... Ignored.
    print = function(...) {
      cat("CD-CAT Session\n")
      cat("  Model    :", self$items$model, "\n")
      cat("  Criterion:", self$criterion, "\n")
      cat("  Items    :", length(self$administered), "/", self$max_items, "\n")
      cat("  Profile  :", self$est$alpha_hat, "\n")
      if (!is.null(self$stop_reason))
        cat("  Stopped  :", self$stop_reason, "\n")
      invisible(self)
    }
  )
)
