pbday <- function(n) {
  ntests <- 1000
  pop <- 1:365
  anydup <- function(i) any(duplicated(sample(pop, n, replace=TRUE)))
  sum(sapply(seq(ntests), anydup)) / ntests
}

x <- 1:100

if (FALSE) {
  # sequential version
  prob <- sapply(x, pbday)
}

if (TRUE) {
  # parallel version
  library(nws)
  s <- sleigh()
  prob <- unlist(eachElem(s, pbday, x))
}

plot(x, prob)

# compare with the analytical results
print(all.equal(prob, sapply(x, pbirthday)))
