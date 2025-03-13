
<!-- README.md is generated from README.Rmd. Please edit that file -->

# seinfitR: Modeling the Relationship Between Nematode Densities and Plant Growth

Authors:

Deoclecio Amorim - <amorim@cena.usp.br>, CENA-USP

João Novoletti - <joao.novoletti@gmail.com>

The goal of seinfitR is to fit the Seinhorst equation to experimental
data describing the relationship between preplant nematode densities and
plant growth using nonlinear least squares fitting.

## Installation

You can install the development version of seinfitR from
[GitHub](https://github.com/dslabcena/seinfitR) with:

``` r
# install.packages("pak")
pak::pak("dslabcena/seinfitR")
```

## Basic Use

The syntax of seinfitR is straightforward, with the main function being
seinfitR(…).

## Example

Modeling plant response to nematode densities using the “jambu” dataset:

``` r
library(seinfitR)

# Fit the model
model <- seinfitR(p_i = "p_i", y = "y", data = jambu,
                  start = list(m = 0.103, t = 250, z = 0.991),
                  control = seinfitR_control(maxiter = 5))
#> It.    0, RSS = 3.08149e-33, Par. =      0.103        250      0.991          1
#> It.    1, RSS = 3.08149e-33, Par. =      0.103        250      0.991          1

# View model summary
summary(model)
#> 
#> Seinhorst Model - Parameter Estimates
#> -----------------------------------------------------
#>       Estimate   Std. Error      t value Pr(>|t|)
#> m        0.103 1.300604e-20 7.919397e+18        0
#> t      250.000 1.961730e-17 1.274385e+19        0
#> z        0.991 2.172494e-21 4.561578e+20        0
#> y_max    1.000 4.956159e-20 2.017691e+19        0
#> -----------------------------------------------------
#> -----------------------------------------------------
```

The seinfitR package provides some methods for model evaluation and
visualization:

``` r
# Print model coefficients
print(model)
#> 
#> Seinhorst Model Fit Summary
#> -----------------------------------------------------
#> Dependent Variable: y 
#> Predictor Variable: p_i 
#> Number of Observations: 5002 
#> 
#>       Estimate   Std. Error      t value Pr(>|t|)
#> m        0.103 1.300604e-20 7.919397e+18        0
#> t      250.000 1.961730e-17 1.274385e+19        0
#> z        0.991 2.172494e-21 4.561578e+20        0
#> y_max    1.000 4.956159e-20 2.017691e+19        0
#> -----------------------------------------------------

# Extract variance-covariance matrix
vcov(model)
#>                   m             t             z         y_max
#> m      1.691571e-40  4.916585e-38 -5.743163e-42 -2.530042e-40
#> t      4.916585e-38  3.848386e-34 -2.868392e-38 -3.028962e-37
#> z     -5.743163e-42 -2.868392e-38  4.719730e-42  1.187502e-51
#> y_max -2.530042e-40 -3.028962e-37  1.187502e-51  2.456351e-39

# Calculate R-squared
r_squared(model)
#> R² (Coefficient of Determination):  1 
#> Adjusted R²:  1

# Calculate Plot
plot(model)
```

<img src="man/figures/README-methods-1.png" width="100%" />

Methods Available for seinfitR Objects

``` r
methods(class = "seinfitR")
#> [1] plot      predict   print     r_squared summary   vcov     
#> see '?methods' for accessing help and source code
```

License

The seinfitR package is licensed under the GNU General Public License,
version 3, see file LICENSE.md. © 2024 Deoclecio J. Amorim & João
Novoletti.
