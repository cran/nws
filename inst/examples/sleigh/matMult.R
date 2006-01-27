matMult = function(s, a, b) {
    vmmult = function(x, y) matrix(x, 1, length(x)) %*% y
    result = eachElem(s, vmmult, list(a), fixedArgs=list(b))
    do.call("rbind", result)
}
