## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(HomomorphicEncryption)
library(polynom)

## ----params-------------------------------------------------------------------
d  =      4
n  =      2^d
p  =     (n/2)-1
q  = 424242
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
# generate the public key
pk0 = GenPubKey0(a, s, e, pm, q)
pk1 = GenPubKey1(a)

## -----------------------------------------------------------------------------
# polynomials for encryption
e1 = GenError(n)
e2 = GenError(n)
u  = GenU(n)

## -----------------------------------------------------------------------------
m1 = polynomial(c(1, 1, 1))
m2 = polynomial(c(0, 1   ))

## -----------------------------------------------------------------------------
m1_ct0 = EncryptPoly0(m1, pk0, u, e1, p, pm, q)
m1_ct1 = EncryptPoly1(    pk1, u, e2,    pm, q)
m2_ct0 = EncryptPoly0(m2, pk0, u, e1, p, pm, q)
m2_ct1 = EncryptPoly1(    pk1, u, e2,    pm, q)

## -----------------------------------------------------------------------------
sum_ct0 = m1_ct0 + m2_ct0
sum_ct1 = m1_ct1 + m2_ct1

sum_ct0 = sum_ct0 %% pm
sum_ct0 = CoefMod(sum_ct0, q)

sum_ct1 = sum_ct1 %% pm
sum_ct1 = CoefMod(sum_ct1, q)

## -----------------------------------------------------------------------------
decrypt = (sum_ct1 * s) + sum_ct0
decrypt = decrypt %% pm
decrypt = CoefMod(decrypt, q)

# rescale
decrypt = decrypt * p/q

# round then mod p
decrypt = CoefMod(round(decrypt), p)
print(decrypt)

