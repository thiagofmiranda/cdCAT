
# cdCAT

<!-- badges: start -->

[![R-CMD-check](https://github.com/thiagofmiranda/cdCAT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thiagofmiranda/cdCAT/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> Computerized Adaptive Testing with Cognitive Diagnostic Models

`cdCAT` is an R package that provides a session-based engine for
**Cognitive Diagnostic Computerized Adaptive Testing (CD-CAT)**. Unlike
simulation-focused packages, `cdCAT` is designed for **real-time,
item-by-item adaptive applications** – making it suitable for
integration with APIs, Shiny apps, and online assessment platforms.

------------------------------------------------------------------------

## Theoretical Framework

`cdCAT` implements adaptive testing under **Cognitive Diagnosis Models
(CDMs)**.

The adaptive cycle follows the standard CD-CAT framework:

1.  Posterior estimation of latent attribute profiles
2.  Item selection based on information criteria
3.  Response update
4.  Stopping rule evaluation

Implemented criteria are grounded in the CD-CAT literature:

- Xu, Chang & Douglas (2003) – KL
- Cheng (2009) – PWKL / MPWKL
- Shannon (1948) – Entropy
- Tatsuoka (2002) – Attribute-level classification
- Hsu, Wang & Chen (2013) – Dual posterior threshold
- Kingsbury & Zara (1991) – Content balancing
- Sympson & Hetter (1985) – Exposure control
- van der Linden (2005) – Shadow CAT

This ensures theoretical consistency with established CD-CAT
methodology.

------------------------------------------------------------------------

## Features

- **Models**: DINA, DINO, and GDINA
- **Item selection criteria**: KL, PWKL, MPWKL, SHE, sequential, random
- **Estimation methods**: MAP, MLE, EAP
- **Custom prior**: informative prior over skill profiles via
  `cdcat_prior()`
- **Content balancing**: blueprint-driven domain proportions (Kingsbury
  & Zara, 1991)
- **Exposure control**: Sympson-Hetter and Randomesque methods
- **Shadow CAT**: user-defined constraint functions for arbitrary test
  assembly
- **Stopping rules**: maximum items, posterior threshold classification
- **Session-based architecture**: stateful R6 object manages the full
  CAT lifecycle
- **Lightweight**: depends only on `R6`

------------------------------------------------------------------------

## Architecture

`cdCAT` is built around a stateful `CdcatSession` R6 object that:

- Stores the full posterior distribution
- Tracks administered items
- Updates responses sequentially
- Evaluates stopping criteria at each step

The item selection pipeline at each step is:

    Content balancing filter  ->  Criterion scores  ->  Exposure control  ->  Selected item
                                        |
                              (Shadow CAT replaces the full pipeline
                               when constr_fun is supplied)

This design makes the package suitable for:

- REST APIs
- Shiny applications
- Online assessment engines
- Real-time adaptive testing systems

------------------------------------------------------------------------

## Installation

``` r
# Development version from GitHub
remotes::install_github("thiagofmiranda/cdCAT")
```

------------------------------------------------------------------------

## Quick Start

``` r
library(cdCAT)

# 1. Define the item bank
Q <- matrix(c(
  1, 0,
  0, 1,
  1, 0,
  0, 1,
  1, 1
), nrow = 5, ncol = 2, byrow = TRUE)

items <- cdcat_items(
  q_matrix = Q,
  model    = "DINA",
  slip     = c(0.10, 0.10, 0.15, 0.10, 0.10),
  guess    = c(0.20, 0.20, 0.15, 0.20, 0.15)
)

# 2. Start a session
session <- CdcatSession$new(
  items     = items,
  criterion = "PWKL",
  max_items = 5L,
  threshold = 0.8
)

# 3. Administer items
simulated_responses <- c(1, 1, 0, 1, 0)

repeat {
  item <- session$next_item()
  if (item == 0) break
  session$update(item, simulated_responses[item])
}

# 4. Results
res <- session$result()
cat("Estimated profile:", res$alpha_hat, "\n")
#> Estimated profile: 1 1
cat("Items used       :", res$n_items, "\n")
#> Items used       : 2
cat("Stop reason      :", res$stop_reason, "\n")
#> Stop reason      : single threshold reached
```

------------------------------------------------------------------------

## Item Selection Criteria

All adaptive criteria operate on the conditional response distribution
P(X_j = 1 \| alpha) and use one of the following strategies:

| Criterion  | Full Name                                                   |
|------------|-------------------------------------------------------------|
| `"KL"`     | Kullback-Leibler Information (KL) Method                    |
| `"PWKL"`   | Posterior-Weighted Kullback-Leibler (PWKL) Method           |
| `"MPWKL"`  | Modified Posterior-Weighted Kullback-Leibler (MPWKL) Method |
| `"SHE"`    | Shannon Entropy (SHE) Method                                |
| `"SEQ"`    | Sequential (non-adaptive)                                   |
| `"RANDOM"` | Random selection                                            |

------------------------------------------------------------------------

## Supported Models

| Model     | Description                         |
|-----------|-------------------------------------|
| `"DINA"`  | Deterministic Input, Noisy And gate |
| `"DINO"`  | Deterministic Input, Noisy Or gate  |
| `"GDINA"` | Generalized DINA                    |

------------------------------------------------------------------------

## Advanced Features

### Custom Prior

Specify an informative prior over skill profiles using `cdcat_prior()`.
Pattern names follow `expand.grid` order (first attribute varies
fastest).

``` r
prior <- cdcat_prior(
  "00" = 0.4,
  "10" = 0.3,
  "01" = 0.2,
  "11" = 0.1
)

session <- CdcatSession$new(items, prior = prior)
```

### Content Balancing

Enforce a test blueprint by restricting each selection step to the most
under-represented content domain (Kingsbury & Zara, 1991).

``` r
content      <- c("Algebra", "Algebra", "Geometry", "Geometry", "Mixed")
content_prop <- c(Algebra = 0.4, Geometry = 0.4, Mixed = 0.2)

session <- CdcatSession$new(
  items        = items,
  content      = content,
  content_prop = content_prop
)
```

### Exposure Control

Limit overuse of specific items with two detection-free methods:

``` r
# Sympson-Hetter: per-item acceptance probabilities in [0, 1]
exposure_sh <- c(0.9, 0.9, 0.9, 0.5, 0.9)   # item 4 accepted only 50% of the time

# Randomesque: top-n pool size per test position (all values >= 1)
exposure_rq <- c(3L, 2L, 2L, 1L, 1L)         # position 1 draws from top-3

session <- CdcatSession$new(items, exposure = exposure_sh)
```

### Shadow CAT

Enforce arbitrary test assembly constraints via a user-supplied
function. `cdCAT` is solver-agnostic: use any optimisation library
inside `constr_fun`.

``` r
# constr_fun receives (scores, items, administered) and returns one item index
greedy_shadow <- function(scores, items, administered) {
  scores[administered] <- -Inf
  which.max(scores)
}

session <- CdcatSession$new(items, constr_fun = greedy_shadow)
```

See `vignette("advanced-item-selection")` for a full LP-based example
using `lpSolve`.

------------------------------------------------------------------------

## How `cdCAT` Differs from Existing Packages

Unlike simulation-oriented packages such as `mirtCAT` or `GDINA`,
`cdCAT` focuses on:

- Real-time session management
- Modular adaptive components (prior, content, exposure, shadow
  independently composable)
- Lightweight dependency structure (only `R6`)
- Explicit posterior tracking at every step
- Solver-agnostic shadow CAT (no `lpSolve` dependency in the package
  core)

------------------------------------------------------------------------

## Roadmap

Planned extensions include:

- Polytomous extensions
- Simulation benchmarking tools
- pkgdown documentation site

------------------------------------------------------------------------

## Authors

- **Thiago Miranda** – <thiagofm.pa@gmail.com>
- **Bruno Silvestre** – <bruno.ap.silvestre@gmail.com>

------------------------------------------------------------------------

## License

MIT
