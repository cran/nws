# first make some data
n <- 1000	# number of obs
p <- 30		# number of variables

x <- matrix(rnorm(n * p), n, p)
beta <- c(rnorm(p / 2, 0, 5), rnorm(p / 2, 0, 0.25))
y <- x %*% beta + rnorm(n, 0, 20)
thedata <- data.frame(y=y, x=x)

summary(lm(y~x))

fold <- rep(1:10, length=n)
fold <- sample(fold)

worker <- function(foldnumber, p) {
    result <- double(p)
    for (i in 1:p) {
        templm <- lm(y~., data=thedata[fold != foldnumber, 1:(i+1)])
        yhat <- predict(templm, newdata=thedata[fold == foldnumber, 1:(i+1)])
	result[i] <- sum((yhat - thedata[fold == foldnumber, 1]) ^ 2)
    }
    mean(result)
}

if (TRUE) {
    srss <- sapply(1:10, worker, p)
    cat('sequential results:', srss, '\n')
}

if (TRUE) {
    library(nws)
    s = sleigh()
    workerinit <- function(d, f) { thedata <<- d; fold <<- f }
    eachWorker(s, workerinit, thedata, fold)
    prss <- unlist(eachElem(s, worker, data.frame(foldnumber=1:10, p=p)))
    cat('parallel results:  ', prss, '\n')
    close(s)
}
