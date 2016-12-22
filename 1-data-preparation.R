setwd("C:\\Users\\ka294056\\Desktop\\IncrementalLearning")
dir.create("input-data", showWarnings = F)
setwd("C:\\Users\\ka294056\\Desktop\\IncrementalLearning\\input-data")

phase1 = function(nrows, name = "train") {
	data = data.frame(A = runif(nrows), B = runif(nrows))
	data$Y = factor(as.numeric(data$A >= 0.6 & data$B >= 0.6))
	write.csv(data, paste0(name, ".csv"), row.names = FALSE)
}

phase2 = function(nrows, name = "test") {
	data = data.frame(A = runif(nrows), B = runif(nrows))
	data$Y = factor(as.numeric(data$A >= 0.8 & data$B >= 0.8))
	write.csv(data, paste0(name, ".csv"), row.names = FALSE)
}


initialFlag = T

if(initialFlag) {
	phase1(10000)
	phase2(1000)
} else {
	# # rename the test-set (if required) to feedback.csv # #
	file.rename("test.csv", "feedback.csv")
	phase2(1000)
}