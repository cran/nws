# configurable parameters
numTasks <- 1000
numSamples <- 10000
numStocks <- 20
chunkSize <- 10
loadFactor <- 3

# hack to make the computation function work sequentially
SleighRank <- 1

# randomly generate the mean and sd that describe each stock
smean <- rnorm(numStocks, mean=10.0, sd=1.0)
ssd <- rnorm(numStocks, mean=3.0, sd=0.5)
stocks <- data.frame(mean=smean, sd=ssd)

# this is the task function, called via eachElem and lappy
fun <- function(numSamples, numStocks) {
  # generate the weights vector
  t <- runif(numStocks)
  w <- t / sum(t)

  # generate random stock returns matrix
  rnormWrapper <- function(i) rnorm(numSamples, mean=stocks$mean[[i]], sd=stocks$sd[[i]])
  s <- do.call(rbind, lapply(1:numStocks, rnormWrapper))

  # do the computation and return the results
  r <- drop(w %*% s)
  c(mean(r), var(r), SleighRank)
}

# do the work in parallel
library(nws); s <- sleigh()
tmp <- eachWorker(s, function(g1) {stocks <<- g1; NULL}, stocks)
opts <- list(chunkSize=chunkSize, loadFactor=loadFactor)
print(system.time(r1 <- eachElem(s, fun, rep(numSamples, numTasks), numStocks, eo=opts)))

# do the work sequentially
print(system.time(r2 <- lapply(rep(numSamples, numTasks), fun, numStocks)))

# clean up
close(s)