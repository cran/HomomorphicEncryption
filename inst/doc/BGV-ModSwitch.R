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
ek0 = GenEvalKey0(a, s, e)
ek1 = a

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
multi_ct0 = m1_ct0 * m2_ct0
multi_ct0 = multi_ct0 %% pm
multi_ct0 = CoefMod(multi_ct0, q)
multi_ct0 = round(multi_ct0)

multi_ct1 = (m1_ct0 * m2_ct1 + m1_ct1 * m2_ct0)
multi_ct1 = multi_ct1 %% pm
multi_ct1 = CoefMod(multi_ct1, q)
multi_ct1 = round(multi_ct1)

multi_ct2 = (m1_ct1 * m2_ct1)
multi_ct2 = multi_ct2 %% pm
multi_ct2 = CoefMod(multi_ct2, q)
multi_ct2 = round(multi_ct2)

## -----------------------------------------------------------------------------
ct0hat = CoefMod(multi_ct0 + ek0 * multi_ct2 %% pm, q)
ct1hat = CoefMod(multi_ct1 + ek1 * multi_ct2 %% pm, q)

## -----------------------------------------------------------------------------
q_prime = q - 1
ct0hat_prime = round(ct0hat * q_prime/q)
ct1hat_prime = round(ct1hat * q_prime/q)

## -----------------------------------------------------------------------------
decrypt = ct0hat_prime + ct1hat_prime * s
decrypt = decrypt %% pm
decrypt = CoefMod(decrypt, q_prime)
decrypt = decrypt * p/q_prime
decrypt = CoefMod(round(decrypt), p)
print(decrypt)

