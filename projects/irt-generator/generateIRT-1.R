## Title: generateIRT-1.R
## Author: Paul E. Johnson <pauljohn@ku.edu>
## Date posted: 2012-11-28
## Depends: None
## Description: Generates 0/1 answer data
## that can be analyzed in an Item Response Theory framework.
## This is one-dimensional "theta" (individual ability).
## One highlight is the use of R's mapply function. This is
## the first instance in which I've found that useful!
## -------------------------------------------------------------------

## Suppose individual abilties are one dimensional, normal
thetas <- rnorm(1000)

## The Lord-Novick specification is logistic probability of
## getting a correct answer, which depends on the question
## parameters a,b and individual ability x.
probLogit <- function(a, b, x){
    exp(a * (x - b))/(1 + exp(a*(x -b)))
}

# We are just doing 5 questions, this is only a demo!
# a b for each question:
as <- c( 1.1, 1.2, 0.7, 0.5, 1.3)
bs <- c( 0.7, 0.5, 0.5, 0.3, 0.1)

## Use mapply process each individual ability parameter.
## The mapply takes 1 person's theta, and then it analyzes each
## question. mapply has the effect of taking a[1],b[1], and then
## a[2],b[2], a[3],b[3], and so forth. Like magic!
## Returns a matrix of probability of correct answer values.
probMat <- mapply(FUN=function(x,y) probLogit(x,y,thetas), as, bs)

## Use a binomial to convert those to 0's and 1's
irtData <- apply(probMat, 2, function(x) rbinom(length(x), 1, x))

## I need somebody to run this generated data through some IRT
## model fitting software to see if it can recover the as and bs.
## And thetas.
## Please.

