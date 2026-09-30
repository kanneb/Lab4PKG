
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Lab4PKG

<!-- badges: start -->

[![R-CMD-check](https://github.com/kanneb/Lab4PKG/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kanneb/Lab4PKG/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Lab4PKG fits a linear regression model using ordinary least squares. You
create the model with `linreg()` and then use its methods to look at the
result.

## Installation

Install from GitHub. Set the option first if you also want the vignette:

``` r
# install.packages("pak")
options(pkg.build_vignettes = TRUE)
pak::pak("kanneb/Lab4PKG")
```

## Example

``` r
library(Lab4PKG)

estimation <- linreg(Petal.Length ~ Species, data = iris)
estimation$print()
#> Call:
#> linreg(formula = Petal.Length ~ Species, data = iris)
#> 
#> Coefficents: 
#>       (Intercept) Speciesversicolor  Speciesvirginica 
#>             1.462             2.798             4.090
```

Other methods: `plot()`, `resid()`, `pred()`, `coef()` and `summary()`.

## More

The vignette explains every method with examples:

``` r
browseVignettes("Lab4PKG")
```
