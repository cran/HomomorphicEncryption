#' Vandermonde
#' @title vandermonde
#' @param xi xi
#' @param M M
#' @return The Vandermonde matrix
#' @export

vandermonde <- function(xi, M) {
  N <- M %/% 2
  # Initialize an empty matrix with complex data type
  v_matrix <- matrix(complex(real = numeric(N * N)), nrow = N, ncol = N)

  for (i in 1:N) {
    root <- xi ^ (2 * (i - 1) + 1)
    for (j in 1:N) {
      v_matrix[i, j] <- root ^ (j - 1)
    }
  }
  return(v_matrix)
}

#' Sigma inverse
#' @title sigma inverse
#' @param xi xi
#' @param M M
#' @param b b
#' @return sigma inverse of xi
#' @export
sigma_inverse <- function(xi, M, b) {
  A <- vandermonde(xi, M)
  coeffs <- solve(A, b)
  return(coeffs)
}

#' Sigma
#' @title sigma function
#' @param xi xi
#' @param M M
#' @param p p
#' @return sigma of xi
#' @export
sigma_function <- function(xi, M, p) {
  outputs <- numeric(N)
  N <- M %/% 2
  for (i in 1:N) {
    root <- xi^(2 * (i - 1) + 1)
    output <- sum(p * root^(0:(N-1)))
    outputs[i] <- output
  }
  return(outputs)
}

#' Sigma discretization
#' @title sigma discretization
#' @param xi xi
#' @param M M
#' @param z z
#' @return sigma R discretization
#' @export
sigma_R_discretization <- function(xi, M, z) {
  sigma_R_basis <- vandermonde(xi, M)
  coordinates <- compute_basis_coordinates(sigma_R_basis, z)
  rounded_coordinates <- coordinate_wise_random_rounding(coordinates)
  y <- sigma_R_basis %*% rounded_coordinates
  return(y)
}

#' Compute basis coordinates
#' @title compute basis coordinates
#' @param sigma_R_basis sigma_R_basis
#' @param z z
#' @return basis coordinates
#' @export
compute_basis_coordinates <- function(sigma_R_basis, z) {
  return(sapply(1:ncol(sigma_R_basis), function(i) {
    b <- sigma_R_basis[, i]
    Re(sum(z * Conj(b)) / sum(b * Conj(b)))
  }))
}

#' Round coordinates
#' @title round coordinates
#' @param coordinates coordinates
#' @return rounded coordinates
#' @export
round_coordinates <- function(coordinates) {
  return(coordinates - floor(coordinates))
}

#' Coordinate-wise random rounding
#' @title coordinate-wise random rounding
#' @param coordinates coordinates
#' @return rounded coordinates
#' @export
coordinate_wise_random_rounding <- function(coordinates) {
  r <- round_coordinates(coordinates)
  f <- sapply(r, function(c) sample(c(c, c-1), 1, prob=c(1-c, c)))
  rounded_coordinates <- coordinates - f
  return(as.integer(rounded_coordinates))
}

#' Pi function
#' @title pi function
#' @param M M
#' @param z z
#' @return Pi of M
#' @export
pi_function <- function(M, z) {
  N <- M %/% 4
  return(z[1:N])
}

#' Pi inverse function
#' @title pi inverse function
#' @param z z
#' @return inverse of z
#' @export
pi_inverse <- function(z) {
  z_conjugate <- Conj(rev(z))
  return(c(z, z_conjugate))
}

#' Encode
#' @title encode
#' @param xi xi
#' @param M M
#' @param scale scale
#' @param z z
#' @return encode polynomial
#' @export
encode <- function(xi, M, scale, z) {
  pi_z <- pi_inverse(z)
  scaled_pi_z <- scale * pi_z
  rounded_scale_pi_zi <- sigma_R_discretization(xi, M, scaled_pi_z)
  p <- sigma_inverse(xi, M, rounded_scale_pi_zi)
  coef <- as.vector(round(Re(p)))
  return(polynomial(coef))
}

#' Decode
#' @title decode
#' @param xi xi
#' @param M M
#' @param scale scale
#' @param p p
#' @return decoded xi
#' @export
decode <- function(xi, M, scale, p) {
  rescaled_p <- coef(p) / scale
  z <- sigma_function(xi, M, rescaled_p)
  return(pi_function(M, z))
}
