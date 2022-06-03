
<!-- README.md is generated from README.Rmd. Please edit that file -->

# impute

NOTE : This package is still in the early stage of development!

This is an R package that wrapps the most popular algorithms for data
imputation. Its objective is to allow the evaluation of these methods in
terms of bias (robustness) and prediction (accuracy).

# Install instruction

``` r
#devtools::install_github("ielbadisy/impute")
# generate_na

irisNA <- impute::generate_na(iris)
irisNAx <- impute::impute(irisNA, "knn") # impute(<incomplete_dataset>, "<imputation_method>")
```

Currently, only the methods bellow are implemented :

-   `naive`: mean/mode  
-   `hotdeck`
-   `knn`
-   `cart`
-   `glmnet`
-   `missranger`
-   `missforest`
-   `spmm`
-   `famd`
-   `mpmm`
-   `micerf`
-   `supermice`
