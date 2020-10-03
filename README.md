![R-CMD-check](https://github.com/MaximilianPi/TraitMatching/workflows/R-CMD-check/badge.svg)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# TraitMatching package

This is a new implementation of the TraitMatching (see https://github.com/TheoreticalEcology/Pichler-et-al-2019 ) package based on mlr3.

More about using machine learning for TraitMatching: https://doi.org/10.1111/2041-210X.13329

To install the package:

```r
devtools::install_github(repo = "https://github.com/MaximilianPi/TraitMatching", subdir = "TraitMatching")
library(TraitMatching)
```

### Workflow

1) simulate data (replace the A, B, and Z matrices with your own):
```r
sim = simulateInteraction(weights = list(main = 0.1, inter = c(0.3,0.3,0.3)))
A = sim$A
B = sim$B
Z = sim$binar()
community = createCommunity(A, B, Z)

```

2) Fit random forest in a nested resampling strategy and with hypertuning:
```r
result = runTM(community = community, method = "RF", iters = 20L)

```

3) Compare different ML algorithms:
```r
result = runTM(community = community, method = c("RF", "SVM", "BRT"), iters = 20L)

```