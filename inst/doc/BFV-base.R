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
q  = 874
pm = GenPolyMod(n)

## -----------------------------------------------------------------------------
set.seed(123)

## -----------------------------------------------------------------------------
# generate a secret key
s = polynomial( sample( c(-1,0,1), n, replace=TRUE ) )
print(s)

## -----------------------------------------------------------------------------
# generate a
a = polynomial(sample.int(q, n, replace=TRUE))
print(a)

## -----------------------------------------------------------------------------
# generate the error
e = polynomial( coef=round(stats::rnorm(n, 0, n/3)) )
print(e)

## -----------------------------------------------------------------------------
# generate the public key
temp = -(a*s + e)
temp = temp %% pm
pk0 = CoefMod(temp, q)
print(pk0)

## -----------------------------------------------------------------------------
pk1 = a

## -----------------------------------------------------------------------------
# create a message
m = polynomial( coef=c(6, 4, 2) )
print(m)

## -----------------------------------------------------------------------------
# polynomials for encryption
e1 = polynomial( coef=round(stats::rnorm(n, 0, n/3)) )
e2 = polynomial( coef=round(stats::rnorm(n, 0, n/3)) )

## -----------------------------------------------------------------------------
u  = polynomial( sample( c(-1,0,1), (n-1), replace=TRUE) )
print(u)

## -----------------------------------------------------------------------------
temp = pk0 * u + e1 + floor(q/p) * m
temp = temp %% pm
ct0 = CoefMod(temp, q)
print(ct0)

## -----------------------------------------------------------------------------
temp = pk1 * u + e2
temp = temp %% pm
ct1 = CoefMod(temp, q)
print(ct1)

## -----------------------------------------------------------------------------
temp = (ct1 * s) + ct0
temp = temp %% pm
temp = CoefMod(temp, q)

# rescale
temp = temp * p/q

## -----------------------------------------------------------------------------
# round then mod p
decrypt = CoefMod(round(temp), p)
print(decrypt)

