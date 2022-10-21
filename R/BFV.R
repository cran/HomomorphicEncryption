#' @name BFV_KeyGen
#' @title Brakerski / Fan-Vercauteren
#' @return polynomial
#' @export
#' @examples
#' d  =      4
#' n  =      2^d
#' p  =     (n/2)-1
#' q  = 424242
#' GenPolyMod(n)

BFV_KeyGen <- function() {
  pk <- list()
  d <-readline('What will be value of d? (leave blank for auto-generated) ')
  d <- as.integer(d)
  if  (is.null(d) )
    d <- 4L # generate it
  n <- 2^d
  p <- (n/2)-1
  pm = GenPolyMod(n)
  q <-readline('What will be value of q? (leave blank for auto-generated) ')
  q <- as.integer(q)
  if ( is.null(q) )
    q <- 874L # generate it
  # generate a secret key
  sk = GenSecretKey(n)
  # generate a
  a = GenA(n, q)
  # generate the error
  e = GenError(n) # this should be generated only within the GenPubKey
  # so that it is never stored anywhere
  # generate the public key
  pk$pk0 = GenPubKey0(a, sk, e, pm, q)
  pk$pk1 = GenPubKey1(a)

  print(c("The public key is: ", pk) )
  return(list(pk=pk,sk=sk))
}

#' @name BFV_encrypt
#' @title BFV encryption
#' @param m message
#' @param pk public key
#' @return polynomial
#' @export
BFV_encrypt <- function(m, pk) {
  # recover n and pm
  n <- length(pk$pk0)
  p  =   (n/2)-1
  pm = GenPolyMod(n)

  e1 = GenError(n) # should be in ct0 generation
  e2 = GenError(n) # should be in ct1 generation
  u  = GenU(n)     # should be in ct0 generation

  ct0 = EncryptPoly0(m, pk$pk0, u, e1, p, pm, 874)
  ct1 = EncryptPoly1(   pk$pk1, u, e2,    pm, 874)
  return(list(ct0=ct0,ct1=ct1))
}


