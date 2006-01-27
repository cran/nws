# matrix addition
# each worker works on one row at a time

library(nws)
source('matAdd.R')

# create a sleigh object with default options
s = sleigh()

rows = 10
cols = 10
n = rows * cols

# we'll use our sleigh to do multiple matrix additions
for (i in 1:10) {
    # generate two random matrices
    A = matrix(rnorm(n), rows, cols)
    B = matrix(rnorm(n), rows, cols)

    # add the matrices using our sleigh
    C = matAdd(s, A, B)

    # check the results
    D = A + B
    if (all.equal(C, D)) {
        cat('Success\n')
    } else {
        cat('*** Failure ***\n')
    }
}

stopSleigh(s)
