# IBHM1 vs IBHM2

Supplementary materials to "Correlation-based method for identification of the structure and parameters of nonlinear regression models" paper.

In order to run the experiment:
1. install `IBHM` packages using [`devtools`](https://cran.r-project.org/web/packages/devtools/index.html) :

```
devtools::install_local('ibhm1')
devtools::install_local('ibhm2')
```


2. run the experimental script (installing some additional packages might be required depending on your R enviroment - namely: `mlr`, `tidyverse`, `xgboost`, `kernlab`,`dummies`) .

```
source('run_benchmark.R')
results = doRun(learners, tasks)
```
