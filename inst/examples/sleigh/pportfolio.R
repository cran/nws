# configurable parameters
numTasks <- 1000
numSamples <- 10000
numStocks <- 20
chunkSize <- 10
loadFactor <- 3
workerCount <- 3

# close all graphics devices that exist, and then create devices 2 and 3
graphics.off()
get(getOption("device"))(width=7, height=7)
get(getOption("device"))(width=7, height=2.5)

# randomly generate the mean and sd that describe each stock
smean <- rnorm(numStocks, mean=10.0, sd=1.0)
ssd <- rnorm(numStocks, mean=3.0, sd=0.5)
stocks <- data.frame(mean=smean, sd=ssd)

# this is the task function, called via eachElem
fun <- function(numSamples, numStocks) {
  # generate the weights vector
  t <- runif(numStocks)
  w <- t / sum(t)

  # generate random stock returns matrix
  rnormWrapper <- function(i) rnorm(numSamples, mean=stocks$mean[[i]], sd=stocks$sd[[i]])
  s <- do.call(rbind, lapply(1:numStocks, rnormWrapper))

  # do the computation and return the results
  r <- drop(w %*% s)
  c(mean(r), var(r), SleighRank + 1)
}

# create the plot window
dev.set(2)
plot.new()
plot.window(xlim=c(0.3, 1.3), ylim=c(9.0, 10.6))
axis(1); axis(2); box()
title(xlab='Risk', ylab='Reward', main='Parallel Efficient Frontier')
colors <- rainbow(workerCount)

# prepare to process the results returned from eachElem
reward <- vector()
risk <- vector()
rindx <- 1
tasksCompleted = integer(workerCount)
dim(tasksCompleted) = c(workerCount, 1)
accum <- function(valueList) {
  results <- unlist(valueList)
  numResults <- length(valueList)
  dim(results) <- c(length(valueList[[1]]), numResults)
  reward[rindx:(rindx + numResults - 1)] <<- results[1,]
  risk[rindx:(rindx + numResults - 1)] <<- results[2,]
  rindx <<- rindx + numResults
  worker = results[3,1]
  dev.set(2)
  axis(1); axis(2); box()
  title(xlab='Risk', ylab='Reward', main='Parallel Efficient Frontier')
  points(results[2,], results[1,], pch=20, cex=0.5, col=colors[worker])
  dev.set(3)
  tasksCompleted[worker,1] <<- tasksCompleted[worker,1] + numResults
  if (rindx > numTasks)
    xlab <- sprintf('Completed all %d Tasks', numTasks)
  else
    xlab <- sprintf('Completed %d of %d Tasks', rindx-1, numTasks)
  barplot(tasksCompleted, xlim=c(0, numTasks), xlab=xlab, horiz=TRUE, col=colors, beside=FALSE)
}

# do the work in parallel
library(nws); s <- sleigh(workerCount=workerCount)
try({library(gtools); setTCPNoDelay(s@nwss@nwsSocket, value=TRUE)}, silent=TRUE)
tmp <- eachWorker(s, function(g1) {stocks <<- g1; NULL}, stocks)
opts <- list(accumulator=accum, chunkSize=chunkSize, loadFactor=loadFactor)
tmp <- eachElem(s, fun, rep(numSamples, numTasks), numStocks, eo=opts)

# determine the efficient frontier
o <- order(risk)
rewardFrontier <- vector()
rewardFrontier[1] <- maxReward <- reward[o[1]]
riskFrontier <- vector()
riskFrontier[1] <- risk[o[1]]
noriskReward <- rewardFrontier[1] - 2  # XXX ???
sharpe <- list(max=(reward[o[1]]-noriskReward)/risk[o[1]], i=1)
i <- j <- 2
while (j <= length(risk)) {
  idx <- o[j]
  t <- reward[idx]
  if (t > maxReward) {
    rewardFrontier[i] <- maxReward <- t
    riskFrontier[i] <- risk[idx]

    tSharpe <- (maxReward - noriskReward) / riskFrontier[i]
    if (tSharpe > sharpe$max) {
      sharpe$max <- tSharpe
      sharpe$i <- i
    }

    i <- i + 1
  }
  j <- j + 1
}

# plot the efficient frontier
dev.set(2)
axis(1); axis(2); box()
title(xlab='Risk', ylab='Reward', main='Parallel Efficient Frontier')
points(riskFrontier, rewardFrontier, type='l', col='black')
lines(c(-10, riskFrontier[sharpe$i], riskFrontier[sharpe$i]),
      c(rewardFrontier[sharpe$i], rewardFrontier[sharpe$i], 0))

# clean up
close(s)
