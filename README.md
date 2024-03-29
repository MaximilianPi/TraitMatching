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
library(TraitMatching)
```

```
## Warning: replacing previous import 'mlr3measures::tnr' by 'mlr3tuning::tnr' when
## loading 'TraitMatching'
```

```r
sim = simulateInteraction(weights = list(main = 0.1, inter = c(0.3,0.3,0.3)))
A = sim$A
B = sim$B
Z = sim$binar()
community = createCommunity(A, B, Z)
```

```
##   missForest iteration 1 in progress...done!
##   missForest iteration 2 in progress...done!
##   missForest iteration 1 in progress...done!
##   missForest iteration 2 in progress...done!
```

2) Fit random forest in a nested resampling strategy and with hyper-parameter tuning:

```r
result = runTM(community = community, method = "RF", iters = 20L)
```


```r
print(result)
```

```
##    nr      resample_result           task_id           learner_id resampling_id
## 1:  1 <ResampleResult[21]> classif_community classif.ranger.tuned        custom
##    iters classif.auc
## 1:    10    0.705656
```


3) Compare different ML algorithms:

```r
result = runTM(community = community, method = c("RF", "SVM"), iters = 3L)
```


```r
print(result)
```

```
##    nr      resample_result           task_id           learner_id resampling_id
## 1:  1 <ResampleResult[21]> classif_community classif.ranger.tuned        custom
## 2:  2 <ResampleResult[21]> classif_community    classif.svm.tuned        custom
##    iters classif.auc
## 1:    10   0.7127029
## 2:    10   0.5529854
```


4) Predict target for new data with different ensemble ML algorithms (trained with optimal hyper-parameters) 

```r
predict(result, newdata=community$data[1:5,])
```

```
##          RF       SVM
## 1 0.8869743 0.7538086
## 2 0.9247219 0.8050551
## 3 0.5959860 0.5702086
## 4 0.7903801 0.5801098
## 5 0.4514330 0.3647977
```
