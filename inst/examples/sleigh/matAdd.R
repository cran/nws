matAdd = function(s, a, b) {
    result = eachElem(s, "+", list(a, b))
    do.call("rbind", result)
}
