# `remstats`

[![github-repo-status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-package-version](https://img.shields.io/github/r-package/v/TilburgNetworkGroup/remstats)](https://www.github.com/TilburgNetworkGroup/remstats)
[![CRAN-release](https://www.r-pkg.org/badges/version/remstats)](https://cran.r-project.org/package=remstats)
[![R-CMD-check](https://github.com/TilburgNetworkGroup/remstats/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/TilburgNetworkGroup/remstats/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/TilburgNetworkGroup/remstats/branch/master/graph/badge.svg?token=BDG8F1672B)](https://codecov.io/gh/TilburgNetworkGroup/remstats)
[![grand-total-downloads](http://cranlogs.r-pkg.org/badges/grand-total/remstats)](https://cran.r-project.org/package=remstats)

## Compute statistics for relational event history data
The `remstats` package is designed to compute a variety of statistics for relational event models. Relational event modeling approaches enable researchers to investigate both exogenous and endogenous factors influencing the evolution of a time-ordered sequence of relational events. These models are categorized into tie-oriented models, where the probability of a dyad interacting next is modeled in a single step (e.g., see Butts, 2008), and actor-oriented models, which first model the probability of a sender initiating an interaction and subsequently the probability of the senders' choice of receiver (e.g., see Stadtfeld & Block, 2017). The `remstats` package is designed to compute a variety of statistics that characterize exogenous and endogenous influences on the event stream for both types of models.

The package is part of a bundle of `R`-packages developed by researchers from Tilburg University intended to aid applied researchers in the application of relational event modeling. For preparing the relational event history, `remstats` assumes the prior application of `remify::remify()` (available on CRAN or on Github at github.com/TilburgNetworkGroup/remify). Model estimation can subsequently be executed using `remstimate` (available on GitHub at github.com/TilburgNetworkGroup/remstimate).

## Installation
To install the package in R, using `devtools`: 
```r
devtools::install_github("TilburgNetworkGroup/remstats")
library(remstats)
```

## Workflow example 
```r
# Load example data
data(history)
data(info)

# Define effects
effects <- ~ 1 + send("extraversion", info) + inertia()

# Prepare event history with the 'remify package' 
# Install with devtools::install_github("TilburgNetworkGroup/remify")
rehObject <- remify::remify(edgelist = history, model = "tie")

# Compute statistics
statsObject <- remstats(reh = rehObject, tie_effects = effects)

# Estimate model parameters with the 'remstimate' package
# Install with devtools::install_github("TilburgNetworkGroup/remstimate")
fit <- remstimate::remstimate(reh = rehObject, stats = statsObject,
    method = "MLE", timing = "interval")
```

## Support 
```r
# To view all vignettes in the remstats package
vignette(package = "remstats")

#To view all help files in the remstats package
help(package="remstats")

#To view available effects for the tie-oriented model
help("all_tie_effects")

#To view available effects for the actor-oriented model
help("all_actor_effects")
```
