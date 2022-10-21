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
m1 = polynomial( coef=c(1, 1, 1) )
m2 = polynomial( coef=c(0, 1   ) )

## -----------------------------------------------------------------------------
# polynomials for encryption
e1 = GenError(n)
e2 = GenError(n)
u  = GenU(n)

## -----------------------------------------------------------------------------
m1_ct0 = pk0*u + p*e1 + m1
m1_ct0 = m1_ct0 %% pm
m1_ct0 = CoefMod(m1_ct0, q)
  
m1_ct1 = pk1*u + p*e2
m1_ct1 = m1_ct1 %% pm
m1_ct1 = CoefMod(m1_ct1, q)

m2_ct0 = pk0*u + p*e1 + m2
m2_ct0 = m2_ct0 %% pm
m2_ct0 = CoefMod(m2_ct0, q)
  
m2_ct1 = pk1*u + p*e2
m2_ct1 = m2_ct1 %% pm
m2_ct1 = CoefMod(m2_ct1, q)

## -----------------------------------------------------------------------------
sum_ct0 = m1_ct0 + m2_ct0
sum_ct1 = m1_ct1 + m2_ct1

## -----------------------------------------------------------------------------
decrypt = (sum_ct1 * s) + sum_ct0
decrypt = decrypt %% pm
decrypt = CoefMod(decrypt, q)
decrypt = CoefMod(round(decrypt), p)
print(decrypt)

