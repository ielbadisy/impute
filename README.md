
<!-- README.md is generated from README.Rmd. Please edit that file -->

# impute

NOTE : Le package est toujours en stade précoce de développement!

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

Acctuallement, seullement les méthodes sont implémentées :

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
