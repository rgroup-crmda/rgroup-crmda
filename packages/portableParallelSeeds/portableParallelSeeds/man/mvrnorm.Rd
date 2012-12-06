\name{mvrnorm}
\alias{mvrnorm}
\title{Replication Fix for draws from a Multivariate Normal Distribution (MASS style)}
\usage{
  mvrnorm(n = 1, mu, Sigma, tol = 1e-06, empirical = FALSE,
    EISPACK = FALSE)
}
\arguments{
  \item{n}{the number of samples ("rows" of data)
  required.}

  \item{mu}{a vector giving the means of the variables.}

  \item{Sigma}{positive-definite symmetric matrix
  specifying the covariance matrix of the variables.}

  \item{tol}{tolerance (relative to largest variance) for
  numerical lack of positive-definiteness in \code{Sigma}}

  \item{empirical}{logical. If true, mu and Sigma specify
  the empirical not population mean and covariance matrix.}

  \item{EISPACK}{logical. Set to true to reproduce results
  from MASS versions prior to 3.1-21.}
}
\value{
  If \code{n = 1} a vector of the same length as \code{mu},
  otherwise an \code{n} by \code{length(mu)} matrix with
  one sample in each row.
}
\description{
  This is the \code{\link[MASS]{mvrnorm}} function from the
  MASS package (Venables and Ripley, 2002), with one small
  modification to facilitate replication of random samples.
  For replication purposes, it is necessary to make sure
  that, after the seed is reset, the first rows of
  generated data are identical no matter what value is
  chosen for n.  That is to say, if one generates k
  observations, and then re-sets the seed, and then
  generates n > k observations, then the first k rows
  should be identical. \code{MASS::mvrnorm} does not meet
  that requirement, but \code{MASS::mvrnorm} does.
}
\details{
  To assure replication, only a very small change is made.
  The code in \code{MASS::mvrnorm} draws a random sample
  and fills a matrix by column, and that matrix is then
  decomposed.  The change implemented here fills that
  matrix by row and the problem is eliminated.

  Some peculiarities are noticed when the covariance matrix
  changes from a diagonal matrix to a more general
  symmetric matrix (non-zero elements off-diagonal).  When
  the covariance is strictly diagonal, then just one column
  of the simulated multivariate normal data will be
  replicated, but the others are not. This has very
  troublesome implications for simulations that draw
  samples of various sizes and then base calculations on
  the separate simulated columns (i.e., some columns are
  identical, others are completely uncorrelated).
}
\examples{
library(portableParallelSeeds)
set.seed(12345)
X0 <- MASS::mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
## create a smaller data set, starting at same position
set.seed(12345)
X1 <- MASS::mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
## Create a larger data set
set.seed(12345)
X2 <- MASS::mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
## The first 5 rows in X0, X1, and X2 are not the same
X0
X1
X2
set.seed(12345)
Y0 <- mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
set.seed(12345)
Y1 <- mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
set.seed(12345)
Y2 <- mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
# note results are the same in the first 5 rows:
Y0
Y1
Y2
identical(Y0[1:5, ], Y1[1:5, ])
identical(Y1[1:5, ], Y2[1:5, ])

myR <- lazyCor(X = 0.3, d = 5)
mySD <- c(0.5, 0.5, 0.5, 1.5, 1.5)
myCov <- lazyCov(Rho = myR, Sd = mySD)

set.seed(12345)
X0 <- MASS::mvrnorm(n=10, mu = rep(0, 5), Sigma = myCov)
## create a smaller data set, starting at same position
set.seed(12345)
X1 <- MASS::mvrnorm(n=5, mu = rep(0, 5), Sigma = myCov)
X0
X1
##' set.seed(12345)
Y0 <- portableParallelSeeds::mvrnorm(n=10, mu = rep(0, 5), Sigma = myCov)
## create a smaller data set, starting at same position
set.seed(12345)
Y1 <- portableParallelSeeds::mvrnorm(n=5, mu = rep(0, 5), Sigma = myCov)
Y0
Y1
}
\author{
  Ripley, B.D. with revision by Paul E. Johnson
}
\references{
  Venables, W. N. & Ripley, B. D. (2002) Modern Applied
  Statistics with S. Fourth Edition. Springer, New York.
  ISBN 0-387-95457-0
}
\seealso{
  For an alternative multivariate normal generator
  function, see the function
  \code{\link[mvtnorm]{rmvnorm}}, in the package
  \code{mvtnorm}.
}

