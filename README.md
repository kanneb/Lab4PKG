
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Lab4PKG

<!-- badges: start -->

[![R-CMD-check](https://github.com/kanneb/Lab4PKG/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kanneb/Lab4PKG/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Lab4PKG fits a linear regression model using ordinary least squares. You
create the model with `linreg()` and then use its methods to look at the
result.

This package also includes the bonus lab, which implements ridge
regression with `ridgereg()`.

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
#> Coefficients: 
#>       (Intercept) Speciesversicolor  Speciesvirginica 
#>             1.462             2.798             4.090
```

Other methods: `plot()`, `resid()`, `pred()`, `coef()` and `summary()`.

### Ridge regression

``` r
ridge <- ridgereg$new(Petal.Length ~ Species, data = iris, lambda = 1)
ridge$coef()
#>       (Intercept) Speciesversicolor  Speciesvirginica 
#>          3.733113          1.303194          1.911567
head(ridge$predict())
#>       [,1]
#> 1 1.467523
#> 2 1.467523
#> 3 1.467523
#> 4 1.467523
#> 5 1.467523
#> 6 1.467523
```

## More

The package has three vignettes:

- **Lab4PKG_vignette**: explains every method of `linreg()` with
  examples.
- **ridgereg**: shows how to use `ridgereg()`.
- **flight_delay**: predicts flight delays from weather data with
  `ridgereg()`.

Open them with:

``` r
browseVignettes("Lab4PKG")
```
