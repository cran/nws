# Taken from http://www.math.ncu.edu.tw/~chenwc/new/myjob/R_note

my.loop <- 20
m.dim <- list(nrow=200000, ncol=10)
m <- matrix(1, nrow=m.dim$nrow, ncol=m.dim$ncol)
ret <- 0

# Sequential version
for (k in 1:my.loop) {
    for (j in 1:m.dim$ncol) {
	for (i in 1:m.dim$nrow) {
	    ret <- ret + m[i,j]
	}
    }
}
cat('sequential version succeeded\n')
print(ret)


# apply version
ret <- 0
for (k in 1:my.loop) {
    ret <- ret + sum(apply(m, 1, sum))
}
cat('apply version succeeded\n')
print(ret)

# parallel (sleigh) version
ret <- 0
library(nws)
sum_func <- function(k) {
    apply(m, 1, sum)
} 

s = sleigh(workerCount=4)
eachWorker(s, function(mat) {m <<- mat}, m)
temp <- eachElem(s, sum_func, list(1:my.loop))
ret <- sum(unlist(temp))
cat('parallel version succeeded\n')
print(ret)
stopSleigh(s)
