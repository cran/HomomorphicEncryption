## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(polynom)
library(HomomorphicEncryption)

## ----seed---------------------------------------------------------------------
set.seed(123)

## ----params-------------------------------------------------------------------
M     <- 8
N     <- M / 2
scale <- 4
xi    <- complex(real = cos(2 * pi / M), imaginary = sin(2 * pi / M))

## ----z------------------------------------------------------------------------
z <- c(complex(real=3, imaginary=4), complex(real=2, imaginary=-1))
print(z)

## ----encode-------------------------------------------------------------------
pi_z                <- pi_inverse(z)
scaled_pi_z         <- scale * pi_z
rounded_scale_pi_zi <- sigma_R_discretization(xi, M, scaled_pi_z)
p                   <- sigma_inverse(xi, M, rounded_scale_pi_zi)
coef                <- as.vector(round(Re(p)))
p                   <- polynomial(coef)

## ----print-p------------------------------------------------------------------
print(p)

## ----decode-------------------------------------------------------------------
rescaled_p <- coef(p) / scale
z          <- sigma_function(xi, M, rescaled_p)
decoded_z  <-pi_function(M, z)

print(decoded_z)

## ----round--------------------------------------------------------------------
round(decoded_z)

