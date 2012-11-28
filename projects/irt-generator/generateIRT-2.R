## Paul E. Johnson
## 2012-11-15

## Draw random scores from a graded response model.

## Some new formulae based on my reading of the "ltm" package for
## R and this article:
## Carlos G. FOrero and Alberto Maydeu-Olivares. 2009.
## Estimation of IRT Graded Response Models: Limited Versus Full
## Information Methods. Psychological Methods 14(3): 275-299.

## this is the "standard" IRT formulation for the probability
## of a correct response. x is "ability", a is discrimination,
## b is difficulty. Right?

probLogit <- function(a, b, x){
    exp(a * (x - b))/(1 + exp(a * (x - b)))
}

## Various sources (Forero and Maydeu-Olivares, for example) claim
## that this equivalent version is more workable, it is is easier to
## extend to MIRT.

## For one question, with ability x, the chance of a correct
## answer depends on alpha and beta.
probLogit <- function(alpha, beta, x){
    exp(alpha + beta * x)/(1 + exp(alpha + beta * x))
}

## CAUTION: signs are changing in unexpected ways here, and
## the labels are changing in confusing ways.
## alpha = -a*b
## beta = a
## Hence the confusing problem that the colloquial a and b
## coefficients seem to swap meanings...
## I know of one dissertation that was blown up by the
## jumbled a's and b's in 2 different programs.

## With a multi-category question, we say there are several
## values of alpha for each question. Think of the alphas as
## thresholds.

## Its easy, then, to see what we need to do. In
## generateIRT-1.R, I used the cumulative probability estimates
## to generate one matrix of answers for N respondents.
## Now I need to generate to generate a cumulative probability matrix
## for each different the threshold parameter

## We have slopes, one for each question:
##  5 questions
betas <- c( 0.7, 0.5, 0.5, 0.3, 0.1)

## For each question, there are thresholds. Suppose
## our items can be assigned 4 grades. That means 3 thresholds.
## threshold 1, for 5 questions
alphas1 <- c( 0.1, 0.2, 0.7, 0.5, 1.3)
## threshold 2, for 5 questions
alphas2 <- c( 0.4, 0.6, 1.0, 0.9, 1.5)
## threshold 3, for 5 questions
alphas3 <- c( 1.7, 1.8, 1.1, 1.0, 1.8)

## Stack those. Note these columns are ordered.
alphas <- rbind(alphas1, alphas2, alphas3)

## How many people?
N <- 10
thetas <- rnorm(N)

## For each person, each question, there are 3 intercepts,
## and we want the cumulative probability of each one.

cumArray <- array(0, dim = c(3, N, ncol(as)))

## I see a way to compress this calculation, but for
## teaching purposes, evaluate each separate threshold
## for each question and person.
##
## Generates a matrix of probability values, for
## the first row of thresholds
cumArray[1, , ] <- mapply(FUN=function(x,y) probLogit(x,y,thetas), alphas[1,], betas)
## Second row of thresholds
cumArray[2, , ] <- mapply(FUN=function(x,y) probLogit(x,y,thetas), alphas[2, ], betas)
## Third row of thresholds
cumArray[3, , ] <- mapply(FUN=function(x,y) probLogit(x,y,thetas), alphas[3, ], betas)

## Those are cumulative probabilities, the chance that the outcome is
## equal to or less than a certain level.

## Use the inversion method to draw samples from an ordinal integer
## variable. I mean, draw a uniform, then "track back" to see where it
## fits among those cumulative probabilities, and then deduce which
## category the observation falls into.

## Look "down" into the cumulative probabilities for
## a given person on a given question, such as this:
## person 3, question 1, has these cumulative probabilities

cumArray[ , 3, 1]
##Suppose that gives
## [1] 0.6036291 0.6727412 0.7350913

## If u < 0.6036291, the outcome is level 1
## If 0.6036291 < u < 0.6727412, outcome is 2.
## If 0.6727412 < u < 0.7350913, outcome is 3.
## If 0.7350913 < u, outcome is 4.

## Here's one way to do that:

invert <- function (y){ 1 + sum(runif(1) > y)  }

(result <- apply(cumArray, c(2,3), function(y){1 + sum(runif(1) > y)} ))

(result <- apply(cumArray, c(2,3), function(y){1 + sum(runif(1) > y)} ))

## Or use the invert function to make it more tidy looking:
(result <-  apply(cumArray, c(2,3), invert))

## Run that a few times, have fun.


## This approach is OK, but I think the
## runif(1) looks dumb, but I don't see a way around it
## without subscripting. Here's what I mean.

##Draw all of the random uniforms for all of the people, question pairs
U <- matrix(runif(5*N), ncol=5, nrow=N)

invert2 <- function(x,y) {1 + sum(U[x,y] > cumArray[ , x, y])}

result <- matrix(0, nrow=N, ncol=5)
for(x in 1:N)
    for(y in 1:5){
     result[x,y] <- invert2(x,y)
 }
result



## Suppose we want correlated answers. What would that mean?
## Would it mean homogenizing the beta coefficients for
## some questions?
## The theta value itself creates some correlation. High
## theta means better answers to all questions. Right?
## But now we want more. questions 2 and 4 to be correlated.
## Perhaps they are affected  by the same exogenous shock.

That's where I'm stumped for a logically consistent approach.

Would correlation between answers to questions 2 and 4 mean that there
is a single random effect added to both of those variables? Think of
it like a clustered random effect, kinda. Sorta.

Does a correlation imply we "boost" both outputs, similar to
simply adding something to the slope coefficient?



