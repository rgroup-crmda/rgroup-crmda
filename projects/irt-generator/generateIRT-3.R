## Title: generateIRT-3.R
## Author: Paul E. Johnson <pauljohn@ku.edu>
## Date posted: 2012-11-28
## Depends: None
## Description: Adapts ideas in generateIRT-1.R and
## generateIRT-2.R to a multidimensional model of individual ability.
## This one is actually fancier than that, it is a very structured
## version of the MIRT, a "bi-factor" model.
##
## The output is dichotomous, 0/1 data.
##  -------------------------------------------------------------------


##x is theta matrix. beta is vector of disc. parameters.
probLogit <- function(alpha, beta, x){
    exp(alpha + x %*% beta)/(1 + exp(alpha + x %*% beta))
}


##npb number of questions per subdomain
genDat <- function(npb = 15, thetas){
    D <- NCOL(thetas)
    N <- NROW(thetas)
    M <- (D-1)*npb

    discrawP <- runif(M, min = 0.6, max = 0.9)
    discrawS <- matrix(runif((D - 1) * M, min = 0.3, max = 0.5), ncol = D - 1)
    discraw <- cbind(discrawP, discrawS)

    blotterMatrix <- matrix(0, nrow = M, ncol = D - 1)
    for (i in 1:(D - 1)) {
        blotterMatrix[(1 + (i - 1) * npb):(i * npb), i] <- 1
    }
    blotterMatrix <- cbind(1, blotterMatrix)
    disc <- discraw * blotterMatrix

    alpha <- runif(M, -2, 1)

## Stupid way, do each question in a for loop:

    probMat <- matrix(NA, N, M)

    for(i in 1:M){
        probMat[ ,i] <- probLogit( alpha[i], t(disc[i, , drop=F]), thetas)
    }

    irtData <- apply(probMat, 2, function(x) rbinom(length(x), 1, x))
    list("probMat"=probMat, "irtData"=irtData)
}


########################################################

library(mvtnorm)
parm <- list()
parm$N <- 1000
parm$D <- 5

sdTheta <- rep(1, parm$D)
rhoThetaV <- diag(parm$D)
sigma <- rhoThetaV * sdTheta %o% sdTheta



genTheta <- function(N, D, sigma){
    mu <- rep(0, D)
    thetas <- rmvnorm(N, mean = mu, sigma = sigma, method= "chol")
}

thetas <- genTheta(parm$N, parm$D, sigma)
simData <- genDat(npb = 15, thetas)


write.table(simData[["irtData"]], file = "dataset2.txt", row.names = FALSE, col.names = FALSE)

