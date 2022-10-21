## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(polynom)
library(HomomorphicEncryption)

## ----params-------------------------------------------------------------------
d  =   4
n  =   2^d
p  =   (n/2)-1
q  = 874
pm = GenPolyMod(n)

## -----------------------------------------------------------------------------
set.seed(123)

## -----------------------------------------------------------------------------
# generate a secret key
s = GenSecretKey(n)

# generate a
a = GenA(n, q)

# generate the error
e = GenError(n)

## -----------------------------------------------------------------------------
pk0 = CoefMod((-a*s + e) %% pm, q)
pk1 = GenPubKey1(a)

## -----------------------------------------------------------------------------
# create a message
m = polynomial( coef=c(1, 1, 1) )

## -----------------------------------------------------------------------------
# polynomials for encryption
e1 = GenError(n)
e2 = GenError(n)
u  = GenU(n)

## -----------------------------------------------------------------------------
ct0 = CoefMod((pk0 * u + e1 + m) %% pm, q)
ct1 = EncryptPoly1(pk1, u, e2, pm, q)

## -----------------------------------------------------------------------------
decrypt = (ct1 * s) + ct0
decrypt = decrypt %% pm
decrypt = CoefMod(decrypt, q)
print(decrypt)

