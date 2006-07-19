# based on ring.py

ring <- function(wsName, numWorkers, numTasks) {
  ws = nwsUseWs(SleighNws@server, wsName)
  mine = sprintf("worker_%d", SleighRank)
  nextOne= (SleighRank + 1) %% numWorkers
  his = sprintf("worker_%d", nextOne)

  for (i in 1:numTasks) {
    j = nwsFetch(ws, mine)
    nwsStore(ws, his, j + 1)
  }
}


# read user input
cat('Please Enter Host Port NumTasks (in order):\n')
input <- scan("", list(host='', port=0, numTasks=0), nmax=1)

host = input$host
if (length(host)==0 || host=="") host = "localhost"

port = as.integer(input$port)
if (length(port)==0 || is.na(port) || port<1)
  port = 8765

numTasks = as.integer(input$numTasks)
if (length(numTasks)==0 || is.na(numTasks) || numTasks < 1)
  numTasks = 10
  

# create a workspace to communicate with workers
server = new('nwsServer', serverHost=host, port=port)
wsName = nwsMktempWs(server)
ws = nwsOpenWs(server, wsName)

s = sleigh(nwsHost=host, nwsPort=port, verbose=TRUE)
numWorkers = nwsFind(s@nws, 'workerCount')

# include the master as one of the workers
numWorkers = numWorkers + 1
cat("Number of workers (include the master): ", numWorkers, "\n")

# tell the workers to execute the ring function defined in this file
eo = list(blocking=FALSE)
eachWorker(s, ring, wsName, numWorkers, numTasks, eo=eo)

# the master becomes the last worker
SleighRank <<- numWorkers - 1
SleighNws <<- s@nws

cat("Master assigned rank ", SleighRank, "\n")

# time how long it takes the token to go all
# the way around the ring numTask times
totalTime = system.time({
  nwsStore(ws, 'worker_0', 0)
  ring(wsName, numWorkers, numTasks)
})[3]

token = nwsFetch(ws, 'worker_0')
stopifnot(token==numTasks*numWorkers)

cat("The token was passed ", token, " times in ", totalTime, " seconds \n")
cat("Seconds per operation: ", totalTime/(2*token), "\n")
  
stopSleigh(s)
close(ws@server)

