rm(list = ls())
gc()
path = "YOUR-PATH-FOR-ALL-THE-R-FILES"
setwd(path)

if("PythonInR" %in% rownames(installed.packages()) == FALSE) {
	install.packages("PythonInR", repos = "http://cran.us.r-project.org/")
} else {
	library(PythonInR)
}
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
nIters = 35
for(run in 1:nIters){
	setwd(path)
	source("3-data-preparation.R")
	# # run-incremental model
	setwd(path)
	source("4-incremental-run.R")
}
