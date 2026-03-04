# data-raw/generate_cdcat_sim.R
#
# Generates `cdcat_sim`: a named list of 16 CD-CAT simulation datasets.
#
# Structure: 2 (K) × 4 (quality) × 2 (model) = 16 elements.
# Each element contains: Q, alpha, parameters, responses.
#
# Naming convention: k{K}_{MODEL}_{QUALITY}
#   K       : 3 or 5
#   MODEL   : DINA, GDINA
#   QUALITY : HDLV, HDHV, LDLV, LDHV
#
# Dependencies (run-time only, not required by the installed package):
#   - GDINA  (simGDINA, attributepattern, extract)
#   - usethis (use_data)
#
# Usage: source("data-raw/generate_cdcat_sim.R")

library(GDINA)

set.seed(2025)

# ------------------------------------------------------------------
# Q-matrix: 10 replicates of each non-zero attribute profile (2^K - 1)
# ------------------------------------------------------------------
make_Q <- function(K) {
  profiles <- as.matrix(expand.grid(replicate(K, 0:1, simplify = FALSE)))
  non_zero <- profiles[rowSums(profiles) > 0, , drop = FALSE]
  Q        <- non_zero[rep(seq_len(nrow(non_zero)), each = 10), ]
  colnames(Q) <- paste0("A", seq_len(K))
  rownames(Q) <- paste0("item", seq_len(nrow(Q)))
  Q
}

# ------------------------------------------------------------------
# Alpha: 20 replicates of each of the 2^K attribute profiles
# ------------------------------------------------------------------
make_alpha <- function(K) {
  profiles <- as.matrix(expand.grid(replicate(K, 0:1, simplify = FALSE)))
  alpha    <- profiles[rep(seq_len(nrow(profiles)), each = 20), ]
  colnames(alpha) <- paste0("A", seq_len(K))
  rownames(alpha) <- paste0("examinee", seq_len(nrow(alpha)))
  alpha
}

# ------------------------------------------------------------------
# Item parameters
#
# Quality controls P0 (non-master correct) and P1 (master correct):
#   HDLV : High Discrimination, Low Variability
#   HDHV : High Discrimination, High Variability
#   LDLV : Low Discrimination, Low Variability
#   LDHV : Low Discrimination, High Variability
#
# DINA/DINO : slip = 1 - P1,  guess = P0  (one draw per item)
# GDINA     : P_min ~ P0, P_max ~ P1; probability increases linearly
#             with the count of mastered required attributes.
# ------------------------------------------------------------------
quality_ranges <- list(
  HDLV = list(P0 = c(0.05, 0.15), P1 = c(0.85, 0.95)),
  HDHV = list(P0 = c(0.00, 0.20), P1 = c(0.80, 0.999)),
  LDLV = list(P0 = c(0.15, 0.25), P1 = c(0.75, 0.85)),
  LDHV = list(P0 = c(0.10, 0.30), P1 = c(0.70, 0.90))
)

make_parameters <- function(Q, model, quality) {
  J        <- nrow(Q)
  P0_range <- quality_ranges[[quality]]$P0
  P1_range <- quality_ranges[[quality]]$P1

  if (model %in% c("DINA", "DINO")) {
    P0    <- runif(J, P0_range[1], P0_range[2])
    P1    <- runif(J, P1_range[1], P1_range[2])
    slip  <- pmin(pmax(1 - P1, 0.001), 0.999)
    guess <- pmin(pmax(P0,     0.001), 0.999)

    lapply(seq_len(J), function(j) list(slip = slip[j], guess = guess[j]))

  } else { # GDINA
    lapply(seq_len(J), function(j) {
      K_j     <- sum(Q[j, ] == 1)
      P_min   <- runif(1, P0_range[1], P0_range[2])
      P_max   <- runif(1, P1_range[1], P1_range[2])
      patterns <- as.matrix(expand.grid(replicate(K_j, 0:1, simplify = FALSE)))

      probs <- vapply(seq_len(nrow(patterns)), function(r) {
        m <- sum(patterns[r, ])
        pmin(pmax(P_min + (m / K_j) * (P_max - P_min), 0.001), 0.999)
      }, numeric(1))

      names(probs) <- apply(patterns, 1, paste0, collapse = "")
      as.list(probs)
    })
  }
}

# ------------------------------------------------------------------
# Response simulation via GDINA::simGDINA()
#
# DINA/DINO : passes gs.parm = data.frame(slip, guess)
# GDINA     : reorders parameters to match attributepattern() canonical
#             order, then passes catprob.parm
# ------------------------------------------------------------------
make_responses <- function(alpha, Q, parameters, model) {
  if (model %in% c("DINA", "DINO")) {
    gs_parm <- data.frame(
      slip  = sapply(parameters, function(p) p$slip),
      guess = sapply(parameters, function(p) p$guess)
    )
    sim <- simGDINA(
      N         = nrow(alpha),
      Q         = Q,
      gs.parm   = gs_parm,
      model     = model,
      attribute = alpha
    )
  } else { # GDINA
    cat_probs <- lapply(seq_along(parameters), function(j) {
      K_j   <- log2(length(parameters[[j]]))
      order <- apply(attributepattern(K_j), 1, paste0, collapse = "")
      unlist(parameters[[j]][order])
    })
    sim <- simGDINA(
      N            = nrow(alpha),
      Q            = Q,
      catprob.parm = cat_probs,
      model        = "GDINA",
      attribute    = alpha
    )
  }

  if (!all(extract(sim, "attribute") == alpha))
    stop("Attributes returned by simGDINA do not match the input alpha matrix.")

  extract(sim, "dat")
}

# ------------------------------------------------------------------
# Generate all 16 datasets
# ------------------------------------------------------------------
K_values  <- c(3L, 5L)
models    <- c("DINA", "GDINA")
qualities <- c("HDLV", "HDHV", "LDLV", "LDHV")

cdcat_sim <- list()

for (K in K_values) {
  Q     <- make_Q(K)
  alpha <- make_alpha(K)

  for (model in models) {
    for (quality in qualities) {
      key <- sprintf("k%d_%s_%s", K, model, quality)
      message("Generating: ", key,
              "  (J = ", nrow(Q), ", N = ", nrow(alpha), ")")

      params    <- make_parameters(Q, model, quality)
      responses <- make_responses(alpha, Q, params, model)

      cdcat_sim[[key]] <- list(
        Q          = Q,
        alpha      = alpha,
        parameters = params,
        responses  = responses
      )
    }
  }
}

# ------------------------------------------------------------------
# Save to data/
# ------------------------------------------------------------------
usethis::use_data(cdcat_sim, overwrite = TRUE)
message("Done. cdcat_sim saved to data/cdcat_sim.rda")
