#' Simulated CD-CAT datasets
#'
#' @description
#' A named list of 16 simulated datasets for Cognitive Diagnostic
#' Computerized Adaptive Testing (CD-CAT) research. The 16 elements
#' cover all combinations of:
#'
#' \itemize{
#'   \item **K** (number of attributes): 3 or 5
#'   \item **Model**: DINA or GDINA
#'   \item **Quality**: HD-LV, HD-HV, LD-LV, or LD-HV
#'     (see *Item quality* section below)
#' }
#'
#' @format
#' A named list with 16 elements. Element names follow the pattern
#' `k{K}_{MODEL}_{QUALITY}`, e.g. `"k3_DINA_HDLV"` or
#' `"k5_GDINA_LDHV"`. Each element is itself a list with four
#' components:
#'
#' \describe{
#'   \item{`Q`}{Integer matrix (\eqn{J \times K}).  Q-matrix mapping items
#'     to attributes. Constructed by replicating each of the
#'     \eqn{2^K - 1} non-zero attribute profiles 10 times, yielding
#'     \eqn{J = 10 \times (2^K - 1)} items
#'     (K = 3: \eqn{J = 70}; K = 5: \eqn{J = 310}).
#'     Rows are named `item1`, ..., `itemJ`; columns `A1`, ..., `AK`.}
#'
#'   \item{`alpha`}{Integer matrix (\eqn{N \times K}). True latent
#'     attribute profiles of the examinees. Constructed by replicating
#'     each of the \eqn{2^K} possible binary profiles 20 times, yielding
#'     \eqn{N = 20 \times 2^K} examinees
#'     (K = 3: \eqn{N = 160}; K = 5: \eqn{N = 640}).
#'     Rows are named `examinee1`, ..., `examineeeN`; columns `A1`, ..., `AK`.}
#'
#'   \item{`parameters`}{List of length \eqn{J}. Item parameters whose
#'     format depends on the model:
#'     \describe{
#'       \item{DINA}{Each element is a list with two scalars:
#'         `slip` (probability that a master answers incorrectly) and
#'         `guess` (probability that a non-master answers correctly).}
#'       \item{GDINA}{Each element is a named list of \eqn{2^{K_j}}
#'         scalars, where \eqn{K_j} is the number of attributes required
#'         by item \eqn{j}. Names are binary strings representing the
#'         latent response pattern (e.g. `"0"`, `"1"` for \eqn{K_j = 1};
#'         `"00"`, `"10"`, `"01"`, `"11"` for \eqn{K_j = 2}).
#'         Each value is the probability of a correct response given
#'         that pattern of required-attribute mastery.}
#'     }
#'   }
#'
#'   \item{`responses`}{Integer matrix (\eqn{N \times J}). Dichotomous
#'     item responses (0 / 1) simulated from `alpha` and `parameters`
#'     using \code{GDINA::simGDINA()}.}
#' }
#'
#' @section Item quality:
#' Parameters were sampled from uniform distributions on ranges that
#' reflect four levels of item quality, defined by the probability of a
#' correct response for non-masters (\eqn{P_0}) and masters (\eqn{P_1}):
#'
#' | Code | Label | \eqn{P_0} range | \eqn{P_1} range |
#' |------|-------|-----------------|-----------------|
#' | `HDLV` | High Discrimination, Low Variability  | \eqn{[0.05, 0.15]} | \eqn{[0.85, 0.95]}  |
#' | `HDHV` | High Discrimination, High Variability | \eqn{[0.00, 0.20]} | \eqn{[0.80, 0.999]} |
#' | `LDLV` | Low Discrimination, Low Variability   | \eqn{[0.15, 0.25]} | \eqn{[0.75, 0.85]}  |
#' | `LDHV` | Low Discrimination, High Variability  | \eqn{[0.10, 0.30]} | \eqn{[0.70, 0.90]}  |
#'
#' For **DINA/DINO**: `slip = 1 - P1` and `guess = P0`, independently
#' sampled per item and clipped to \eqn{[0.001, 0.999]}.
#'
#' For **GDINA**: \eqn{P_{min} \sim P_0} and \eqn{P_{max} \sim P_1}
#' are independently sampled per item; the probability of a correct
#' response increases linearly with the count of mastered required
#' attributes: \eqn{P_{min} + (m / K_j) \times (P_{max} - P_{min})},
#' where \eqn{m} is the number of required attributes mastered.
#'
#' @section Reproducibility:
#' All datasets were generated with `set.seed(2025)`. The generation
#' script is available in `data-raw/generate_cdcat_sim.R`.
#'
#' @section Dataset dimensions:
#' | K | J (items) | N (examinees) |
#' |---|-----------|---------------|
#' | 3 | 70        | 160           |
#' | 5 | 310       | 640           |
#'
#' @examples
#' # List all available datasets
#' names(cdcat_sim)
#'
#' # Access a specific dataset
#' d <- cdcat_sim[["k3_DINA_HDLV"]]
#' dim(d$Q)          # 70 x 3
#' dim(d$alpha)      # 160 x 3
#' dim(d$responses)  # 160 x 70
#'
#' # Inspect DINA parameters for the first item
#' d$parameters[[1]]
#'
#' # Inspect GDINA parameters for the first item
#' cdcat_sim[["k3_GDINA_HDLV"]]$parameters[[1]]
#'
#' # Use in a CD-CAT session
#' d <- cdcat_sim[["k3_DINA_HDLV"]]
#' items <- cdcat_items(
#'   d$Q, "DINA",
#'   slip  = sapply(d$parameters, `[[`, "slip"),
#'   guess = sapply(d$parameters, `[[`, "guess")
#' )
#' session <- CdcatSession$new(items, criterion = "PWKL", max_items = 10L)
#'
#' @source
#' Generated with \code{GDINA::simGDINA()} (Ma & de la Torre, 2020).
#' See \code{data-raw/generate_cdcat_sim.R} for the full script.
#'
#' @references
#' Ma, W., & de la Torre, J. (2020). GDINA: An R package for cognitive
#' diagnosis modeling. \emph{Journal of Statistical Software}, \emph{93}(14),
#' 1--26. \doi{10.18637/jss.v093.i14}
#'
#' @keywords datasets
"cdcat_sim"
