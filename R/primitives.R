#' @name GenPolyMod
#' @title Generate Polynomial Modulo
#' @param n the order
#' @return polynomial of the form x^^n + 1
#' @import polynom
#' @importFrom HEtools CoefMod
#' @export
HEtools::CoefMod

#' @name GenSecretkey
#' @title Generate Secret key
#' @param n the order
#' @return polynomial of order x^^n with coefficients (-1,-,1)
#' @export
#' @examples
#' n = 16
#' GenSecretKey(n)
GenSecretKey <- function(n)
  polynomial( sample.int(3, n, replace=TRUE)-2 )

#' @name GenA
#' @title Generate a
#' @param n the order
#' @param q the ciphermod of coefficients
#' @return polynomial of order x^^n with coefficients 0,..,q
#' @export
#' @examples
#' n = 16
#' q =  7
#' GenA(n, q)
GenA <- function(n, q)
  polynomial(sample.int(q, n, replace=TRUE))

#' @name GenError
#' @title Generate a
#' @param n the order
#' @return polynomial of order x^^n with discrete Gaussian distribution, bounded (not strictly true) by -n,n
#' @import stats
#' @export
#' @examples
#' n = 16
#' GenError(n)
GenError <- function(n)
  polynomial( coef=round(stats::rnorm(n, 0, n/3)) )

#' @name GenU
#' @title Generate u
#' @param n the order
#' @return polynomial of order x^^n-1 with coefficients (-1,-,1)
#' @export
#' @examples
#' n = 16
#' GenU
GenU <- function(n) {
  coefs = sample.int(3, (n-1), replace=TRUE)-2
  polynomial( coef=coefs )
}

#' @name GenPubKey0
#' @title Generate part 0 of the Public Key
#' @param a a
#' @param s s
#' @param e e
#' @param pm pm
#' @param q q
#' @return polynomial
#' @export
GenPubKey0 <- function(a, s, e, pm, q) {
  temp = -(a*s + e) # e should be generated in here, not fed into it
  temp = temp %% pm
  temp = CoefMod(temp, q)
  return(temp)
}

#' @name GenPubKey1
#' @title Generate part 1 of the Public Key
#' @param a a
#' @return polynomial
#' @export
GenPubKey1 <- function(a)
  return(a)

#' @name GenEvalKey0
#' @title Generate the Evaluation Key
#' @param a a
#' @param s s
#' @param e e
#' @return polynomial
#' @export
GenEvalKey0 <- function(a, s, e)
  -(a*s + e) + s^2


#' @name GenPubKey
#' @title Generate the Public Key
#' @param a a
#' @param n n
#' @param e e
#' @param pm pm
#' @param q q
#' @return list with the two polynomials that form the public key
#' @export
GenPubKey <- function(a, n, e, pm, q) {
  temp     = list()
  temp$pk0 = GenPubKey0(a, n, e, pm, q)
  temp$pk1 = GenPubKey1(a          )
  return(temp)
}

#' @name EncryptPoly0
#' @title Encrypt Polynomial Message Part 0
#' @param m message
#' @param pk0 public key part 0
#' @param u u
#' @param e1 e1
#' @param p p
#' @param pm pm
#' @param q q
#' @return polynomial which contains the message in ciphertext
#' @export
EncryptPoly0 <- function(m, pk0, u, e1, p, pm, q) {
  temp = pk0 * u + e1 + floor(q/p) * m
  temp = temp %% pm
  temp = CoefMod(temp, q)
  return(temp)
}

#' @name EncryptPoly1
#' @title Encrypt Polynomial Message Part 1
#' @param pk1 public key part 1
#' @param u u
#' @param e2 e2
#' @param pm pm
#' @param q q
#' @return polynomial which contains the message in ciphertext
#' @export
EncryptPoly1 <- function(pk1, u, e2, pm, q) {
  temp = pk1 * u + e2
  temp = temp %% pm
  temp = CoefMod(temp, q)
  return(temp)
}
