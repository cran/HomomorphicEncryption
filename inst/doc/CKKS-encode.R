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
p <- encode(xi, M, scale, z)

## ----print-p------------------------------------------------------------------
print(p)

## ----decode-------------------------------------------------------------------
decoded_z <- decode(xi, M, scale, p)
print(decoded_z)

## ----round--------------------------------------------------------------------
round(decoded_z)

