d  =   4
n  =   2^d
p  =   (n/2)-1
q  = 874
pm = GenPolyMod(n)

set.seed(123)

# generate a secret key
s = GenSecretKey(n)

# generate a
a = GenA(n, q)

# generate the error
e = GenError(n)

GenPubKey(a,n,e,pm,q)

pk0 = GenPubKey0(a, s, e, pm, q)
pk1 = GenPubKey1(a)

m = polynomial( coef=c(300, 400, 500) )

e1 = GenError(n)
e2 = GenError(n)
u = GenU(n)

ct0 = EncryptPoly0(m, pk0, u, e1, p, pm, q)
ct1 = EncryptPoly1(   pk1, u, e2,    pm, q)

decrypt = (ct1 * s) + ct0
decrypt = decrypt %% pm
decrypt = CoefMod(decrypt, q)

# rescale
decrypt = decrypt * p/q

decrypt = CoefMod(round(decrypt), p)

test_that("multiplication works", {
  expect_equal(as.vector(decrypt)[1], 4)
})

set.seed(123)
a <- BFV_KeyGen()
expect_equal(sum(a$pk$pk0[]),3939566)
expect_equal(sum(BFV_encrypt(15,a$pk)$ct0[]),4953)
