#' Select the next item in a CD-CAT session
#'
#' Computes the item selection criterion score for all available items
#' and returns the index of the best candidate.
#'
#' @param items A `cdcat_items` object.
#' @param responses Numeric vector of 0/1/NA responses (length = n_items).
#' @param administered Integer vector of already administered item indices.
#' @param criterion A string: `"KL"`, `"PWKL"`, `"MPWKL"`, `"SHE"`,
#'   `"random"`, or `"seq"`.
#' @param est A list returned by `estimate_alpha()`. If `NULL`, it is computed
#'   internally.
#' @param method Estimation method passed to `estimate_alpha()` if `est = NULL`.
#' @param prior Prior probabilities passed to `estimate_alpha()` if `est = NULL`.
#'
#' @return Integer scalar — index of the selected item.
#' @export
select_next_item <- function(
    items,
    responses,
    administered,
    criterion,
    est    = NULL,
    method = "MAP",
    prior  = NULL
) {

  criterion <- toupper(criterion)

  # Itens disponíveis
  available <- setdiff(seq_len(items$n_items), administered)

  if (length(available) == 0)
    return(0L)

  # Seleção não-adaptativa
  if (criterion == "SEQ")
    return(available[1])

  if (criterion == "RANDOM")
    return(available[sample(length(available), 1)])

  # Estimação (se não fornecida)
  if (is.null(est)) {
    est <- estimate_alpha(responses, items, method = method, prior = prior)
  }

  prob_matrix     <- est$prob_matrix
  posterior       <- est$posterior
  alpha_hat_index <- est$alpha_hat_index

  # Scores adaptativos
  scores <- sapply(available, function(j) {
    switch(
      criterion,
      KL    = KL_criteria(j, alpha_hat_index, prob_matrix),
      PWKL  = PWKL_criteria(j, alpha_hat_index, prob_matrix, posterior),
      MPWKL = MPWKL_criteria(j, prob_matrix, posterior),
      SHE   = -SHE_criteria(j, prob_matrix, posterior),  # menor entropia = melhor
      stop(sprintf("Unknown criterion: '%s'.", criterion))
    )
  })

  available[which.max(scores)]
}


