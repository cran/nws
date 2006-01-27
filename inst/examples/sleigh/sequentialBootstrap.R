# bootstrapping example taken from SNOW tutorial
# This example can also be found in boot package's help menu

source('nuclearBootstrapInit.R')

timing<-system.time(nuke.boot<-boot(nuke.data, nuke.fun, R=20000, m=1, 
		    fit.pred=new.fit, x.pred=new.data))[3]

cat("bootstrap nuclear data: ", timing, "\n\n")
