n <- 1000  # rows of x
p <- 30    # columns of x
x <- matrix(rnorm(n * p), n, p)
beta <- c(rnorm(p / 2, 0, 5), rnorm(p / 2, 0, 0.25))
y <- x %*% beta + rnorm(n, 0, 20)
dat <- data.frame(y=y, x=x)
fold <- rep(1:10, length=n)
fold <- sample(fold)

worker <- function(foldnumber, p) {
    fun <- function(i) {
        lmfit <- lm(y ~ ., data=dat[fold != foldnumber, 1:(i+1)])
        yhat <- predict(lmfit, newdata=dat[fold == foldnumber, 1:(i+1)])
        sum((yhat - dat[fold == foldnumber, 1]) ^ 2)
    }
    mean(sapply(1:p, fun))
}

# Sequential version
if (TRUE) {
    srss <- sapply(1:10, worker, p)
    cat('sequential results:', srss, '\n')
}

# Parallel version
if (TRUE) {
    library(nws); s = sleigh()
    workerinit <- function(d, f) { dat <<- d; fold <<- f }
    eachWorker(s, workerinit, dat, fold)
    prss <- unlist(eachElem(s, worker, data.frame(foldnumber=1:10, p=p)))
    cat('parallel results:  ', prss, '\n')
}
