##' Convert the vech (column of strictly lower trianglar values from a matrix) into a correlation matrix.
##'
##' vech2Corr is a convenience function for creating correlation matrices
##' from a vector of the lower triangular values. It checks the arguments
##' to make sure they are consistent with the requirements of a
##' correlation matrix. All values must be in [-1, 1], and the number
##' of values specified must be correct for a lower triangle.
##'
##' Use this in combination with the \code{covMat} function to
##' convert a vector of standard deviations and the correlation matrix
##' into a covariance matrix.
##'
##' @export
##' @seealso Similar functions exist in many packages, see  \code{vec2sm} in corpcor, \code{xpnd} in MCMCpack
##' @param vech A vector of values to be placed into the strictly lower triangle of a matrix. All values must be in the [0,1] interval (because they are correlations).
##' @return A symmetric correlation matrix, with 1's on the diagonal.
##' Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' v <- c(0.1, 0.4, -0.9)
##' vech2Corr(v)
##' v <- c(0.1, 0.4, -0.9, 0.4, 0.5, 0.1)
##' vech2Corr(v)
##'
vech2Corr <- function(vech){
    ##compute number of rows from vech. diag not in the vech!
    n = (sqrt(1 + 8 * length(vech)) + 1)/2
    if (!as.integer(n) == n) stop(deparse(substitute(vech)), " must have the correct number of elelemnts to fill in a strictly lower triangle in a square matrix.")
    if(any(vech > 1 | vech < -1)) stop("All values in ", deparse(substitute(vech)), " must be in the interval [-1,1]")
    X <- matrix(NA, nrow = n, ncol = n)
    X[lower.tri(X, diag = FALSE)] <- vech
    X[upper.tri(X)] <- t(X)[upper.tri(X)]
    diag(X) <- 1
    X
}

NULL


NULL

##' Create covariance matrix from correlation and standard deviation information
##'
##' This is a flexible function that allows lazy R programmers to create covariance matrix. The user may be lazy because the correlation and standard deviation infomation may be supplied in a variety of formats.
##'
##' @param Rho Required. May be a single value (correlation common among all variables), a vector of the lower triangular values (vech) of a correlation matrix, or a symmetric matrix of correlation coefficients.
##' @param Sd Required. May be a single value (standard deviation common among all variables) or a vector of standard deviations, one for each variable.
##' @param d Optional. lazyCov will try to manufacture correlation and standard deviation matrices and vectors from minimal information, but the required dimension of the final matrix may be needed when the user supplies only a single value for both Rho and Sd.
##' @return covariance matrix suitable for input into mvrnorm.
##' @author <pauljohn@@ku.edu>
##' @export
##' @examples
##' ##correlation 0.8 for all pairs, standard deviation 1.0 of each
##' lazyCov(Rho = 0.8, Sd = 1.0, d = 3)
##' ## supply a vech (lower triangular values in a column)
##' lazyCov(Rho = c(0.1,0.2,0.3), Sd = 1.0)
##' ## supply vech with different standard deviations
##' lazyCov(Rho = c(0.1,0.2,0.3), Sd = c(1.0, 2.2, 3.3))
##' newRho <- lazyCor(c(0.5,0.6, 0.7, -0.1, -0.5, 0.2))
##' lazyCov(Rho = newRho, Sd = 1.0)
##' lazyCov(Rho = newRho, Sd = c(3,4,5,6))
lazyCov <- function(Rho, Sd, d){
    if (missing(Sd)) stop("lazyCov requires user to specify either a vector or a single common value for all standard deviations")
    if (missing(Rho)) stop("lazyCov requires a symmstric correlation matrix or enough information to create one, either a vech of lower triangular values or a single common correlation value")
    if (!missing(d) && (length(Sd) > 1) && (length(Sd) != d)) stop("lazyCov doesn't require a d argument, but if you provide one, it must be consistent with the length of a supplied Sd vector")
    if (missing(d)){
        if (length(Sd) > 1) d <- length(Sd)
        else if (is.matrix(Rho)) d <- NROW(Rho)
        else if (is.vector(Rho)) {
            d <- (sqrt(1 + 8 * length(Rho)) + 1)/2
            if (!isTRUE(all.equal(as.integer(d)- d, 0))) stop(deparse(substitute(vech)), " must have the correct number of elelemnts to fill in a strictly lower triangle in a square matrix.")
        }
    }
    if (length(Sd) == 1) Sd <- rep(Sd, d)
    Rho <- lazyCor(Rho, d)

    covMat <- diag(Sd) %*% Rho %*% diag(Sd)
    covMat
}





##' Create correlation matrices.
##'
##' Use can supply either a single value (the common correlation among
##' all variables), a column of the lower triangular values for a
##' correlation matrix, or a candidate matrix. The function will check
##' X and do the right thing. If X is a matrix, check that it
## is a valid correlation matrix. If its a single value, use that
## to fill up a matrix. If itis a vector, try to use it as a vech
## to fill the lower triangle..
##' @param X Required. May be one value, a vech, or a matrix
##' @param d Optional. The number of rows in the correlation matrix to
##' be created. lazyCor will deduce the desired size from X if
##' possible. If X is a single value, d is a required argument.
##' @return A correlation matrix.
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' lazyCor(0.5, 8)
##' lazyCor(c(0.1, 0.2, 0.3))
##' lazyCor(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
##' lazyCor(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.10))
lazyCor <- function(X, d) {
    if (is.matrix(X)){
        stopifnot (isSymmetric(X))
        if (!dim(X)[1] == d) stop("lazyCor: the dimension of the matrix supplied is inconsistent with the dimension argument d")
    } else if (length(X) == 1) {
        if ( X < -1 | X > 1 ) stop(paste("The value of of a correlation should be in [-1,1]"))
        X <- matrix(X, nrow = d, ncol = d)
        diag(X) <- 1.0
    } else if (is.vector(X)){
        X <- vech2Corr(X)
    } else {
        stop(paste("lazyCor cannot understand the value supplied for argument", deparse(substitute(X)),".\n That should be either a", d, " x ", d, "symmetric matrix, \n or a vech of the strictly lower triangular part of a matrix, or \n one single value, which we will use to fill up a matrix."))
    }
    X
}

