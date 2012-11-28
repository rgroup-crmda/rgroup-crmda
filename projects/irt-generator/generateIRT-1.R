## Paul E. Johnson
## 2012-11-15

## Generate 0-1 IRT data for one theta dimension
## and as many a and b parameters as you specify

thetas <- rnorm(1000)

probLogit <- function(a, b, x){
    exp(a * (x - b))/(1 + exp(a*(x -b)))
}

#a b for each question:
as <- c( 1.1, 1.2, 0.7, 0.5, 1.3)
bs <- c( 0.7, 0.5, 0.5, 0.3, 0.1)

##go through as and bs, giving each one ability value
## Generates a matrix of probabilty values
probMat <- mapply(FUN=function(x,y) probLogit(x,y,thetas), as, bs)

## Use a binomial to convert those to 0's and 1's

irtData <- apply(probMat, 2, function(x) rbinom(length(x), 1, x))



