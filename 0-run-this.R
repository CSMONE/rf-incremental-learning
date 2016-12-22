rm(list = ls())
gc()
path = "C:\\Users\\ka294056\\Desktop\\IncrementalLearning"
setwd(path)


library(PythonInR)
pyConnect()
pyExecfile("clean-all.py")
pyExit()
# # prepare - training and unseen test set
source("1-data-preparation.R")
# # build an initial model
setwd(path)
source("2-initial-run.R")
# # # REPEAT THE BELOW STEPS # # #
# # prepare - new unseen test set and make the old test set as feedback set
nIters = 5 #35
for(run in 1:nIters){
	setwd(path)
	source("3-data-preparation.R")
	# # run-incremental model
	setwd(path)
	source("4-incremental-run.R")
}