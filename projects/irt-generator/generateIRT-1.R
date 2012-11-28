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

set.seed(12345)
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

## If you don't want to figure out how to do that, why bother
## reading this example?



## ---------------------------stop stop stop-----------##
## Study the previous, make sure it is doing what you want.
## Then go on...


## Supposing that works, I'd wrap it in a function like so.
## Student users tell me they might want both the IRT 0/1
## data and the probability matrix correct answers, so I
## return both in a named list.

genIRTData <- function(theta, avec, bvec){
 if(missing(theta)) stop("theta argument is required")
 if(missing(avec)) stop ("a argument is required")
 if(missing(bvec)) stop ("b argument required")

 probMat <- mapply(FUN=function(x,y) probLogit(x,y,theta), avec, bvec)
 irtData <- apply(probMat, 2, function(x) rbinom(length(x), 1, x))
 list("irtData" = irtData, "probMat" = probMat)
}


## For replication:
set.seed(444444)

## Make more questions
as <- c( 1.1, 1.2, 0.7, 0.5, 1.3, 0.7, 0.5, 0.7, 0.9, 1.4)
bs <- c( 0.7, 0.5, 0.5, 0.3, 0.1, 0.5, 0.4, 0.8, 0.2, 0.6)
sample1 <- genIRTData(thetas, as, bs)
sirt1 <- sample1$irtData ## review the answers

## Use ltm to fit the IRT model, ask for traditional parameterization
## so we can compare against as and bs.
library(ltm)
sirt1.ltm <- ltm( sirt1~ z1, IRT.param = TRUE)

summary(sirt1.ltm)
as
bs
## Not bad, right?? The estimates from ltm are almost exactly correct.
## In the IRT.param=TRUE setting, the
## as are Discrimination,
## bs are difficulty

##---------------------now another variation----------------##

## It just occurred to me that "probLogit" is not necessarily the
## only function we'd want to use for the probability of a correct answer.
## We will want a guessing effect at some point. We might want a C-log-log
## CDF.
##
## Lets work on a Normal CDF first.
## We supposed Logistic before, but it could be many other formulae.
## Here's an example using the standard normal CDF. x has to be scaled
## on the standard normal in order for this to work.
probNorm <- function(a, b, x){
    pnorm(a * (x - b), 0, 1)
}


## Revise the genIRTData function to require user to supply
## a probability model. So far, the only options are probLogit
## and probNorm, so use one of those

genIRTData <- function(theta, avec, bvec, pmodel){
 if(missing(theta)) stop("theta argument is required")
 if(missing(avec)) stop ("a argument is required")
 if(missing(bvec)) stop ("b argument required")

 probMat <- mapply(FUN=function(x,y) pmodel(x,y,theta), avec, bvec)
 irtData <- apply(probMat, 2, function(x) rbinom(length(x), 1, x))
 list("irtData" = irtData, "probMat" = probMat)
}


## First, resimulate to verify that gives same result as previous
## version which assumed probLogit.
set.seed(444444)
sample1A <- genIRTData(thetas, as, bs, probLogit)
sirt1A <- sample1$irtData
## Matches previous, right?
identical(sample1$probMat, sample1A$probMat)
identical(sample1$irtData, sample1A$irtData)
##OK!

## Now draw a fresh sample, but use the normal CDF
set.seed(444444)
sample2 <- genIRTData(thetas, as, bs, probNorm)
sirt2 <- sample2$irtData ## review the answers

##fit that
sirt2.ltm <- ltm( sirt2 ~ z1, IRT.param = TRUE)

summary(sirt2.ltm)
as
bs


## You are the IRT experts, tell me what sirt2.ltm means. Compare
coef(summary(sirt1.ltm))
coef(summary(sirt2.ltm))

## I did not grasp the translation of scales problem between
## logistic and normal very well.  It seems to me that when
## I specified the as and bs vectors, I should have the re-scaled
## when shifting from probLogit to probNorm. Right?

## I found it a bit easier to change to the "other" parameterization
## to compare these.

sirt1.ltm2 <- ltm( sirt1 ~ z1, IRT.param = FALSE)

sirt2.ltm2 <- ltm( sirt2 ~ z1, IRT.param = FALSE)


## Exercise for the reader:

## 1. Make new functions that introduce "guessing" in probLogit and probNorm.

## 2. Write code to make some graphs that compare the "true" as and bs with
## the estimate as and bs.

## 3. How well do these models do in estimating abilties? You have the
## thetas, tell me what you are finding out.
