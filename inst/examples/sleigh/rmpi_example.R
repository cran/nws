# This is a simple 10-fold cross-validation example. 
# It represents a common class of problems where a common set of 
# instructions is run given slightly different data, and the results are 
# collected and operated upon. 
# These problems tend to be classified as "embarassingly parallel."
#
# This is the original single-machine code. It creates a list of 1000 random 
# samples with 30 predictor variables, with more predictive value being placed 
# on the first 15. There's randomization inserted here to make sure the algorithm 
# actually works. Then, 10-fold cross-validation is done over predicting linear models 
# over the first i predictor variables, where i ranges from 1 to 30. 
# The rss values resulting from the cross-validation are calculated and displayed graphically.
#
# example and description taken from 
# http://ace.acadiau.ca/math/ACMMaC/Rmpi/examples.html


# first make some data
n <- 1000    # number of obs
p <- 30      # number of variables


# initialize data
x <- matrix(rnorm(n * p), n, p)
beta <- c(rnorm(p / 2, 0, 5), rnorm(p / 2, 0, 0.25))
y <- x %*% beta + rnorm(n, 0, 20)
thedata <- data.frame(y=y, x=x)

summary(lm(y~x))
fold <- rep(1:10, length=n)
fold <- sample(fold)


task_func <- function(i, j, sd) {
  # specify a seed to have deterministic results
  if (!missing(sd)) set.seed(sd)
  templm <- lm(y~., data=thedata[fold != j, 1:(i+1)])
  yhat <- predict(templm, newdata=thedata[fold == j, 1:(i+1)])
  sum((yhat - thedata[fold == j, 1]) ^ 2)
}

seed <- 123

# parallel version
cat('Parallel version\n')
library(nws)
s <- sleigh()
t <- eachWorker(s, function(d, f) { thedata <<- d; fold <<- f }, thedata, fold)
j <- rep(1:10, each=p)
i <- rep(1:p, 10)
eo = list(chunkSize = 50)
temp_result <- eachElem(s, task_func, list(i, j), list(seed), eo=eo)
prssresult <- matrix(unlist(temp_result), p, 10)
print(prssresult)

# sequential version
cat('Sequential version\n')
srssresult <- matrix(0, p, 10)
for (j in 1:10)
  for (i in 1:p)
    srssresult[i, j] <- task_func(i, j, seed)
print(srssresult)

if (!identical(prssresult, srssresult)) {
  cat('sequential and parallel results are different\n')
}


# this plot shows cross-validated residual sum of squares versus
# the model number. As expected, the most important thing is including
# the first p/2 of the predictors

# plot(apply(prssresult, 1, mean))
