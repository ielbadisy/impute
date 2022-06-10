
<!-- README.md is generated from README.Rmd. Please edit that file -->

# imputer

<!-- badges: start -->
<!-- badges: end -->

The goal of imputer is to give a consistant yet direct way to
impute/ampute a data set using a variety set of imputation algorihms (see
?imputer::imputer).

## Installation

You can install the development version of imputer from
[GitHub](https://github.com/ielbadisy/imputer) with:

``` r
#install.packages("devtools")
devtools::install_github("ielbadisy/imputer")
```

## Example

This is a basic example which shows you how to solve a common problem:

**ampute**

``` r
library(imputer)
#> Loading required package: survival
## basic example code

irisNA <- generate_na(iris)
head(irisNA)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4          NA  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1           NA         0.2  setosa
#> 5          5.0         3.6          1.4          NA  setosa
#> 6          5.4          NA          1.7         0.4  setosa
```

and **impute**

``` r
iriscomp <- imputer(irisNA, "knn")
head(iriscomp)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.3  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.6          1.4         0.3  setosa
#> 6          5.4         3.5          1.7         0.4  setosa
```
