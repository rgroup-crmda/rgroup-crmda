##' Replication Fix for draws from a Multivariate Normal Distribution (MASS style)
##'
##' This is the \code{\link[MASS]{mvrnorm}} function from the MASS
##' package (Venables and Ripley, 2002), with one small modification
##' to facilitate replication of random samples. For replication
##' purposes, it is necessary to make sure that, after the seed is
##' reset, the first rows of generated data are identical no matter
##' what value is chosen for n.  That is to say, if one generates k
##' observations, and then re-sets the seed, and then generates n > k
##' observations, then the first k rows should be
##' identical. \code{MASS::mvrnorm} does not meet that requirement, but \code{MASS::mvrnorm} does.
##'
##' To assure replication, only a very small change is made. The code
##' in \code{MASS::mvrnorm} draws a random sample and fills a matrix by column,
##' and that matrix is then decomposed.  The change implemented here fills that matrix by row and the problem is eliminated.
##'
##' Some peculiarities are noticed when the covariance matrix changes from a diagonal matrix to a more general symmetric matrix (non-zero elements off-diagonal).  When the covariance is strictly diagonal, then just one column of the simulated multivariate normal data will be replicated, but the others are not. This has very troublesome implications for simulations that draw samples of various sizes and then base calculations on the separate simulated columns (i.e., some columns are identical, others are completely uncorrelated).
##'
##' @seealso For an alternative multivariate normal generator function, see the function \code{\link[mvtnorm]{rmvnorm}}, in the package \code{mvtnorm}.
##' @param n the number of samples ("rows" of data) required.
##' @param mu a vector giving the means of the variables.
##' @param Sigma positive-definite symmetric matrix specifying the
##'    covariance matrix of the variables.
##' @param tol tolerance (relative to largest variance) for numerical lack
##'    of positive-definiteness in \code{Sigma}
##' @param empirical logical. If true, mu and Sigma specify the empirical
##'    not population mean and covariance matrix.
##' @param EISPACK logical. Set to true to reproduce results from MASS
##'    versions prior to 3.1-21.
##' @import MASS
##' @export
##' @return If \code{n = 1} a vector of the same length as \code{mu}, otherwise an
##'  \code{n} by \code{length(mu)} matrix with one sample in each row.
##' @author Ripley, B.D. with revision by Paul E. Johnson
##' @references
##' Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with
##' S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0
##' @examples
##'
##' library(portableParallelSeeds)
##' set.seed(12345)
##' X0 <- MASS::mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
##' ## create a smaller data set, starting at same position
##' set.seed(12345)
##' X1 <- MASS::mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
##' ## Create a larger data set
##' set.seed(12345)
##' X2 <- MASS::mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
##' ## The first 5 rows in X0, X1, and X2 are not the same
##' X0
##' X1
##' X2
##' set.seed(12345)
##' Y0 <- mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
##' set.seed(12345)
##' Y1 <- mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
##' set.seed(12345)
##' Y2 <- mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
##' # note results are the same in the first 5 rows:
##' Y0
##' Y1
##' Y2
##' identical(Y0[1:5, ], Y1[1:5, ])
##' identical(Y1[1:5, ], Y2[1:5, ])
##'
##' myR <- lazyCor(X = 0.3, d = 5)
##' mySD <- c(0.5, 0.5, 0.5, 1.5, 1.5)
##' myCov <- lazyCov(Rho = myR, Sd = mySD)
##'
##' set.seed(12345)
##' X0 <- MASS::mvrnorm(n=10, mu = rep(0, 5), Sigma = myCov)
##' ## create a smaller data set, starting at same position
##' set.seed(12345)
##' X1 <- MASS::mvrnorm(n=5, mu = rep(0, 5), Sigma = myCov)
##' X0
##' X1
##' ##' set.seed(12345)
##' Y0 <- portableParallelSeeds::mvrnorm(n=10, mu = rep(0, 5), Sigma = myCov)
##' ## create a smaller data set, starting at same position
##' set.seed(12345)
##' Y1 <- portableParallelSeeds::mvrnorm(n=5, mu = rep(0, 5), Sigma = myCov)
##' Y0
##' Y1
##'
mvrnorm <-
    function(n = 1, mu, Sigma, tol=1e-6, empirical = FALSE, EISPACK = FALSE)
{
    p <- length(mu)
    if(!all(dim(Sigma) == c(p,p))) stop("incompatible arguments")
    if (missing(EISPACK)) EISPACK <- getOption("mvnorm_use_EISPACK", FALSE)
    eS <- eigen(Sigma, symmetric = TRUE, EISPACK = EISPACK)
    ev <- eS$values
    if(!all(ev >= -tol*abs(ev[1L]))) stop("'Sigma' is not positive definite")
    X <- matrix(rnorm(p * n), n, byrow = TRUE)
    if(empirical) {
        X <- scale(X, TRUE, FALSE) # remove means
        X <- X %*% svd(X, nu = 0)$v # rotate to PCs
        X <- scale(X, FALSE, TRUE) # rescale PCs to unit variance
    }
    X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% t(X)
    nm <- names(mu)
    if(is.null(nm) && !is.null(dn <- dimnames(Sigma))) nm <- dn[[1L]]
    dimnames(X) <- list(nm, NULL)
    if(n == 1) drop(X) else t(X)
}
NULL

##' Replication Fix for draws from a Multivariate Normal Distribution (mvtnorm style)
##'
##' This is the \code{\link[mvtnorm]{rmvnorm}} function from the \code{mvtnorm}
##' package, with one small modification
##' to facilitate replication of random samples. The aim is to make
##' sure that, when the seed is reset to a common position, the first
##' rows of generated data are identical no matter what value is
##' chosen for n.  That is to say, if one generates k observations,
##' and then re-sets the seed, and then generates n > k observations,
##' then the first k rows in both will be identical. This may be
##' necessary in Monte Carlo simulations that adjust the sample size,
##' leaving other variables unchanged.
##'
##' @return A matrix with one column per variable, one row per draw.
##' @import mvtnorm
##' @export
##' @author Friedrich Leisch and Fabian Scheipl with revision by Paul Johnson
##' @param n Number of observations.
##' @param mean Mean vector, default is 0 for each variable.
##' @param sigma Covariance matrix, default is diagonal with variance equal to 1.
##' @param method Matrix decomposition used to determine the matrix
##' root of \code{sigma}, possible methods are eigenvalue
##' decomposition \code{"eigen"}, default), singular value
##' decomposition (\code{"svd"}), and Cholesky decomposition
##' (\code{"chol"}).
##  @seealso MASS package \code{\link[MASS]{mvrnorm}}
##' @references
##' Alan Genz, Frank Bretz, Tetsuhisa Miwa, Xuefei Mi, Friedrich Leisch,
##' Fabian Scheipl, Torsten Hothorn (2012). mvtnorm: Multivariate Normal
##' and t Distributions. R package version 0.9-9993. URL
##' http://CRAN.R-project.org/package=mvtnorm
##'
##' Alan Genz, Frank Bretz (2009), Computation of Multivariate Normal and
##' t Probabilities. Lecture Notes in Statistics, Vol. 195.,
##' Springer-Verlage, Heidelberg. ISBN 978-3-642-01688-2
##'
##' @examples
##' library(portableParallelSeeds)
##' set.seed(12345)
##' X0 <- mvtnorm::rmvnorm(n=10, mean = c(0,0,0), sigma = diag(3))
##' ## create a smaller data set, starting at same position
##' set.seed(12345)
##' X1 <- mvtnorm::rmvnorm(n=5, mean = c(0,0,0), sigma = diag(3))
##' ## Create a larger data set
##' set.seed(12345)
##' X2 <- mvtnorm::rmvnorm(n=15, mean = c(0,0,0), sigma = diag(3))
##' ## The first 5 rows in X0, X1, and X2 are not the same
##' X0
##' X1
##' X2
##' set.seed(12345)
##' Y0 <- mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
##' set.seed(12345)
##' Y1 <- mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
##' set.seed(12345)
##' Y2 <- mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
##' # note results are the same in the first 5 rows:
##' Y0
##' Y1
##' Y2
##' identical(Y0[1:5, ], Y1[1:5, ])
##' identical(Y1[1:5, ], Y2[1:5, ])
##'
##' myR <- lazyCor(X = 0.3, d = 5)
##' mySD <- c(0.5, 0.5, 0.5, 1.5, 1.5)
##' myCov <- lazyCov(Rho = myR, Sd = mySD)
##'
##' set.seed(12345)
##' X0 <- mvtnorm::rmvnorm(n = 10, sigma = myCov)
##' ## create a smaller data set, starting at same position
##' set.seed(12345)
##' X1 <-  mvtnorm::rmvnorm(n = 5, sigma = myCov)
##' X0
##' X1
##' set.seed(12345)
##' Y0 <- portableParallelSeeds::mvrnorm(n=10, mu = rep(0, 5), Sigma = myCov)
##' ## create a smaller data set, starting at same position
##' set.seed(12345)
##' Y1 <- portableParallelSeeds::mvrnorm(n=5, mu = rep(0, 5), Sigma = myCov)
##' Y0
##' Y1
##'
rmvnorm <- function (n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)), method = c("eigen", "svd", "chol")) {
    if (!isSymmetric(sigma, tol = sqrt(.Machine$double.eps),
                     check.attributes = FALSE)) {
        stop("sigma must be a symmetric matrix")
    }
    if (length(mean) != nrow(sigma)) {
        stop("mean and sigma have non-conforming size")
    }
    sigma1 <- sigma
    dimnames(sigma1) <- NULL
    if (!isTRUE(all.equal(sigma1, t(sigma1)))) {
        warning("sigma is numerically not symmetric")
    }
    method <- match.arg(method)
    if (method == "eigen") {
        ev <- eigen(sigma, symmetric = TRUE)
        if (!all(ev$values >= -sqrt(.Machine$double.eps) * abs(ev$values[1]))) {
            warning("sigma is numerically not positive definite")
        }
        retval <- ev$vectors %*% diag(sqrt(ev$values), length(ev$values)) %*%
            t(ev$vectors)
    }
    else if (method == "svd") {
        sigsvd <- svd(sigma)
        if (!all(sigsvd$d >= -sqrt(.Machine$double.eps) * abs(sigsvd$d[1]))) {
            warning("sigma is numerically not positive definite")
        }
        retval <- t(sigsvd$v %*% (t(sigsvd$u) * sqrt(sigsvd$d)))
    }
    else if (method == "chol") {
        retval <- chol(sigma, pivot = TRUE)
        o <- order(attr(retval, "pivot"))
        retval <- retval[, o]
    }
    retval <- matrix(rnorm(n * ncol(sigma)), nrow = n, byrow = TRUE) %*% retval
    retval <- sweep(retval, 2, mean, "+")
    colnames(retval) <- names(mean)
    retval
}




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

