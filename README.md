
# cdCAT

<!-- badges: start -->

[![R-CMD-check](https://github.com/thiagofmiranda/cdCAT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thiagofmiranda/cdCAT/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> Computerized Adaptive Testing with Cognitive Diagnostic Models

`cdCAT` is an R package that provides a session-based engine for
**Cognitive Diagnostic Computerized Adaptive Testing (CD-CAT)**. Unlike
simulation-focused packages, `cdCAT` is designed for **real-time,
item-by-item adaptive applications** — making it suitable for
integration with APIs, Shiny apps, and online assessment platforms.

## Features

- **Models**: DINA, DINO, and GDINA
- **Item selection criteria**: KL, PWKL, MPWKL, SHE, sequential, random
- **Estimation methods**: MAP, MLE, EAP
- **Stopping rules**: maximum items, posterior threshold classification
- **Session-based**: stateful R6 object manages the full CAT lifecycle
- **Lightweight**: depends only on `R6`

## Installation

``` r
# Development version from GitHub
remotes::install_github("thiagofmiranda/cdCAT")
```

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
#> Estimated profile: 0 1
cat("Items used       :", res$n_items, "\n")
#> Items used       : 5
cat("Stop reason      :", res$stop_reason, "\n")
#> Stop reason      : max_items reached
```

## Item Selection Criteria

| Criterion  | Description                                |
|------------|--------------------------------------------|
| `"PWKL"`   | Posterior-weighted KL divergence (default) |
| `"KL"`     | KL divergence from estimated profile       |
| `"MPWKL"`  | Mutual posterior-weighted KL divergence    |
| `"SHE"`    | Shannon entropy reduction                  |
| `"SEQ"`    | Sequential (non-adaptive)                  |
| `"RANDOM"` | Random selection                           |

## Supported Models

| Model     | Description                         |
|-----------|-------------------------------------|
| `"DINA"`  | Deterministic Input, Noisy And gate |
| `"DINO"`  | Deterministic Input, Noisy Or gate  |
| `"GDINA"` | Generalized DINA                    |

## Authors

- **Thiago Miranda** — <thiagofm.pa@gmail.com>
- **Bruno Silvestre** — <brunoapsilvestre@gmail.com>

## License

MIT
