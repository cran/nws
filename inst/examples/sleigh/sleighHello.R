# print n number of hello world from workers

library(nws)

# change launch if you add nodeList parameter
s = sleigh()

cat('Type a positive number:\n')
n = scan("", n=1, quiet=TRUE)

fun = function(x) paste("hello world", x, "from worker", SleighRank)
greetings = eachElem(s, fun, 1:n)

print(greetings)

stopSleigh(s)
