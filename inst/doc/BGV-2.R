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

## -----------------------------------------------------------------------------
set.seed(123)

## ----GenPolyMod---------------------------------------------------------------
pm = polynomial( coef=c(1, rep(0, n-1), 1 ) )
print(pm)

## ----secretkey----------------------------------------------------------------
# generate a secret key
s = polynomial( sample.int(3, n, replace=TRUE)-2 )
print(s)

## ----a------------------------------------------------------------------------
# generate a
a = polynomial(sample.int(q, n, replace=TRUE))
print(a)

## ----e------------------------------------------------------------------------
# generate the error
e = polynomial( coef=round(stats::rnorm(n, 0, n/3)) )
print(e)

## ----pubkey1------------------------------------------------------------------
pk1 = -(a*s + p*e)
pk1 = pk1 %% pm
pk1 = CoefMod(pk1, q)
print(pk1)

## ----pubkey2------------------------------------------------------------------
pk2 = a

## -----------------------------------------------------------------------------
# create a message
m = polynomial( coef=c(2, 3, 4) )

## ----e1e2u--------------------------------------------------------------------
# polynomials for encryption
e1 = polynomial( coef=round(stats::rnorm(n, 0, n/3)) )
e2 = polynomial( coef=round(stats::rnorm(n, 0, n/3)) )
u  = polynomial( coef=sample.int(3, (n-1), replace=TRUE)-2 )
print(u)

## ----ciphertext1--------------------------------------------------------------
ct1 = pk1*u + p*e1 + m
ct1 = ct1 %% pm
ct1 = CoefMod(ct1, q)
print(ct1)

## ----ciphertext2--------------------------------------------------------------
ct2 = pk2*u + p*e2
ct2 = ct2 %% pm
ct2 = CoefMod(ct2, q)
print(ct2)

## -----------------------------------------------------------------------------
decrypt = (ct2 * s) + ct1
decrypt = decrypt %% pm
decrypt = CoefMod(decrypt, q)
decrypt = CoefMod(round(decrypt), p)
print(decrypt)

