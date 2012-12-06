library(portableParallelSeeds)

projSeeds <- seedCreator(50, 3, seed = 123456)
A1 <- projSeeds[[27]]
A1 ## shows states of 3 generators for one run

## This function does not need to know what the run number is,
## since it accepts a stream collection as its first argument.
## Prehaps this is a simpler, more elegant approach than in initPortableStreams.
runOneSimulation <- function(streams, N, m, sd, beta = c(0.3, 0.2, 0.1)){
    setSeedCollection(streams)

    datX <- portableParallelSeeds::mvrnorm(N, mu = rep(m, 3), Sigma = sd * diag(3))
    colnames(datX) <- c("X1", "X2", "X3")
    useStream(2)
    eta <- datX %*% beta
    SigmaE <- sqrt(mean(eta))
    Y1 <- eta + SigmaE * rnorm(N)
    datX <- data.frame(datX, Y1)
    lm1 <- lm(Y1 ~ X1 + X2 + X3, data = datX)
    ## lets see a log link, just for fun
    datX$Y2 <- rpois(N, lambda = exp(eta))
    lm2 <- lm(Y2 ~ X1 + X2 + X3, data = datX)
    lm3 <- lm(log(Y2+0.5) ~ X1 + X2 + X3, data = datX)
    glm3 <- glm(Y2 ~ X1 + X2 + X3,  data = datX, family = poisson(link="log"))
    list("lm1" = lm1, "lm2" = lm2, "lm3" = lm3, "glm3" = glm3)
}


serial1 <- lapply(projSeeds[1:30], runOneSimulation, N = 1000, m = 2, sd = 10, beta = c(0.5, 0.6, 0.1))

lapply(serial1[[27]], summary)

## Recall A1 was created above. Repeat that run, explicitly:
serial1.27 <- runOneSimulation(A1, N = 1000, m = 2, sd = 10,  beta = c(0.5, 0.6, 0.1))
summary(serial1[[27]][[2]])

identical(coef(serial1.27[[1]]), coef(serial1[[27]][[1]]))
identical(coef(serial1.27[[2]]), coef(serial1[[27]][[2]]))
sapply(serial1[[2]], coef)

