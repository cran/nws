# matrix multiplication
# each worker works on a row of the result matrix
#
# entire matrix B is sent as a fixed argument and one row of
# matrix A is sent to worker for computing one row of the result matrix

library(nws)
source('matMult.R')

# change launch if you add nodeList parameter
s = sleigh()

rows = 10
cols = 10
n = rows * cols

# we'll use our sleigh to do multiple matrix multiplications
for (i in 1:10) {
    # generate two random matrices
    A = matrix(rnorm(n), rows, cols)
    B = matrix(rnorm(n), cols, rows)

    # multiply the matrices using our sleigh
    C = matMult(s, A, B)

    # check the results
    D = A %*% B
    if (isTRUE(all.equal(C, D))) {
        cat('Success\n')
    } else {
        cat('*** Failure ***\n')
    }
}

stopSleigh(s)
