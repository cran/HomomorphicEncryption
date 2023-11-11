## ----include = FALSE----------------------------------------------------------
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
t  =   p
q  = 868
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
pk0 = GenPubKey0(a, s, e*p, pm, q)
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
ct0 = pk0*u + p*e1 + m
ct0 = ct0 %% pm
ct0 = CoefMod(ct0, q)
  
ct1 = pk1*u + p*e2
ct1 = ct1 %% pm
ct1 = CoefMod(ct1, q)

## -----------------------------------------------------------------------------
decrypt = (ct1 * s) + ct0
decrypt = decrypt %% pm
decrypt = CoefMod(decrypt, q)
decrypt = CoefMod(round(decrypt), p)
print(decrypt)

