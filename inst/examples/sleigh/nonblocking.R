# based on nonblocking.py
# simple program that multiplies value by 2.

worker <- function (wsName) {
  ws = nwsUseWs(SleighNws@server, wsName)
  while (1) {
    task = nwsFetch(ws, 'task')
    result = 2 * task
    nwsStore(ws, 'result', c(task, result))
  }
}


# read user input
input <- readline('Please enter number of tasks:\n')
numTasks <- as.integer(input)
if (is.na(numTasks) || numTasks < 1)
  stop("Please enter a positive number")

# create a workspace to communicate with the workers
server = new('nwsServer')
wsName = nwsMktempWs(server)
ws = nwsOpenWs(server, wsName)

# change launch if you add nodeList parameter
s = sleigh()

eo = list(blocking=FALSE)

eachWorker(s, worker, wsName, eo=eo)

for (i in 1:numTasks)
  nwsStore(ws, 'task', i)

for (i in 1:numTasks) {
  result = nwsFetch(ws, 'result')
  cat(result[1], ' times 2 is ', result[2], '\n')
}
  
nwsClose(ws)
stopSleigh(s)
