pbootstrapHelper <- function(repNo, repCounts,
                             data, statistic, sim, stype,
                             strata, L, m, weights,
                             ran.gen, mle, ...)
{
    require("boot", quietly=TRUE)
    nreps <- repCounts[repNo+1] - repCounts[repNo]
    boot(data, statistic, nreps, sim=sim, stype=stype, strata=strata,
            L=L, m=m, weights=weights, ran.gen=ran.gen, mle=mle, ...)
}

pbootstrap <- function(sl, nchunks,
                       data, statistic, R, sim="ordinary", stype="i",
                       strata=rep(1, n), L=NULL, m=0, weights=NULL,
                       ran.gen=function(d, p) d, mle=NULL, ...)
{
    require("boot", quietly=TRUE)
    thisCall <- match.call()
    repCounts <- floor(R * 0:nchunks / nchunks)
    if (length(dim(data)) == 2) n <- nrow(data)
    else n <- length(data)

    res <- eachElem(sl, pbootstrapHelper, list(1:nchunks),
            list(repCounts, data, statistic, sim, stype, strata,
            L, m, weights, ran.gen, mle, ...))

    out <- NULL
    for (i in res) {
        if (is.null(out)) {
            out <- c(i)
            class(out) <- "boot"
        }
        else {
            out$R <- out$R + i$R
            out$t <- array(c(out$t, i$t), c((nrow(out$t) + nrow(i$t)), 1))
        }
    }

    if (! is.null(out))
        out$call <- thisCall
    out
}
