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
scale <- 200
xi    <- complex(real = cos(2 * pi / M), imaginary = sin(2 * pi / M))

## ----z------------------------------------------------------------------------
z <- c(complex(real=3, imaginary=4), complex(real=2, imaginary=-1))
print(z)

## ----encode-------------------------------------------------------------------
m <- encode(xi, M, scale, z)

## ----print-p------------------------------------------------------------------
print(m)

## ----params2------------------------------------------------------------------
d  =   4
n  =   2^d
p  =   (n/2)-1
q  = 874
pm = GenPolyMod(n)

## ----seckey-------------------------------------------------------------------
# generate a secret key
s = GenSecretKey(n)

# generate a
a = GenA(n, q)

# generate the error
e = GenError(n)

## ----pubkey-------------------------------------------------------------------
pk0 = GenPubKey0(a, s, e, pm, q)
pk1 = GenPubKey1(a)

## -----------------------------------------------------------------------------
# polynomials for encryption
e1 = GenError(n)
e2 = GenError(n)
u  = GenU(n)

## -----------------------------------------------------------------------------
ct0 = CoefMod((pk0*u + e1 + m) %% pm, q)
ct1 = EncryptPoly1(pk1, u, e2, pm, q)

## -----------------------------------------------------------------------------
decrypt = (ct1 * s) + ct0
decrypt = decrypt %% pm
decrypt = CoefMod(decrypt, q)
print(decrypt[1:length(m)])

## ----decode-------------------------------------------------------------------
decoded_z <- decode(xi, M, scale, polynomial(decrypt[1:length(m)]))
print(decoded_z)

## ----round--------------------------------------------------------------------
round(decoded_z)

