# based on pping.py


ping <- function (ws, totalTasks) {
  for (i in 1:totalTasks) {
    r = nwsFetch(ws, 'ping')
    stopifnot(length(r)==2)
    nwsStore(ws, r[1], r[2])
  }
}

pong <- function (wsName, numTasks, size) {
  # use global variable SleighNws and SleighRank,
  # which are accessible in the workerloop
  ws = nwsUseWs(SleighNws@server, wsName)
  pong = sprintf("pong_%d", SleighRank)
  cat(pong, '\n')
  payload = paste(rep("#", size), collapse='')
  cat(payload, '\n')

  for (i in 1:numTasks) {
    nwsStore(ws, 'ping', c(pong, payload))
    j = nwsFetch(ws, pong)
  }
}


# read user input
cat('Please Enter Host Port NumTasks Size (in order):\n')
input <- scan("", list(host="", port=0, numTasks=0, size=0), nmax=1)

host = input$host
if (length(host)==0 || host=="") host = "localhost"

port = as.integer(input$port)
if (length(port)==0 || is.na(port) || port<1)
  port = 8765

numTasks = as.integer(input$numTasks)
if (length(numTasks)==0 || is.na(numTasks) || numTasks < 1)
  numTasks = 10
  
size = as.integer(input$size)
if (length(size)==0 || is.na(size) || size < 1)
  size = 10


# create a workspace to communicate with workers
server = new('nwsServer', serverHost=host, port=port)
wsName = nwsMktempWs(server)
ws = nwsOpenWs(server, wsName)


# create a Sleigh and compute the number of workers
s = sleigh(nwsHost=host, nwsPort=port, verbose=TRUE)

numWorkers = nwsFind(s@nws, 'workerCount')

eo = list(blocking=FALSE)
eachWorker(s, pong, wsName, numTasks, size, eo=eo)

totalTasks = numWorkers*numTasks
totalTime = system.time(ping(ws, totalTasks))[3]
cat("\n\nSeconds per operation: ", totalTime / (4*totalTasks), '\n')
cat("Payload size is approximately ", size, " bytes\n\n")

nwsClose(ws)
stopSleigh(s)
