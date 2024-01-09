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
M     <-   8
N     <-   M / 2
scale <- 200
xi    <- complex(real = cos(2 * pi / M), imaginary = sin(2 * pi / M))

## ----z------------------------------------------------------------------------
z <- c(complex(real=3, imaginary=4), complex(real=2, imaginary=-1))
print(z)

## ----encode-------------------------------------------------------------------
pi_z                <- pi_inverse(z)
scaled_pi_z         <- scale * pi_z
rounded_scale_pi_zi <- sigma_R_discretization(xi, M, scaled_pi_z)
m                   <- sigma_inverse(xi, M, rounded_scale_pi_zi)
coef                <- as.vector(round(Re(m)))
m                   <- polynomial(coef)

## ----print-m------------------------------------------------------------------
print(m)

## ----params2------------------------------------------------------------------
n  =  16
p  =   7
q  = 874
pm = polynomial( coef=c(1, rep(0, n-1), 1 ) )

## ----seckey-------------------------------------------------------------------
# generate a secret key
s = polynomial( sample.int(3, n, replace=TRUE)-2 )

# generate a
a = polynomial(sample.int(q, n, replace=TRUE))

# generate the error
e = polynomial( coef=round(stats::rnorm(n, 0, n/3)) )

## ----pubkey-------------------------------------------------------------------
pk0 = CoefMod(-(a*s +e)%%pm,q)
pk1 = a

## -----------------------------------------------------------------------------
# polynomials for encryption
e1 = polynomial( coef=round(stats::rnorm(n, 0, n/3)) )
e2 = polynomial( coef=round(stats::rnorm(n, 0, n/3)) )
u  = polynomial( coef=sample.int(3, (n-1), replace=TRUE)-2 )

## -----------------------------------------------------------------------------
ct0 = CoefMod((pk0*u + e1 + m) %% pm, q)
ct1 = CoefMod((pk1*u + e2    ) %% pm, q)

## -----------------------------------------------------------------------------
decrypt <- (ct1 * s) + ct0
decrypt <- decrypt %% pm
decrypt <- CoefMod(decrypt, q)
print(decrypt[1:length(coef(m))])

## ----decode-------------------------------------------------------------------
rescaled_p <- decrypt[1:length(m)] / scale
z          <- sigma_function(xi, M, rescaled_p)
decoded_z  <- pi_function(M, z)

print(decoded_z)

## ----round--------------------------------------------------------------------
round(decoded_z)

