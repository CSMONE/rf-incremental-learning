##################################################################################
######################## Stage - 2: Go through read-me.txt #######################
##################################################################################

# Stage:2
# -------
# - Start the iterations[iter = 1 to n]
# - Assumption is that the feedback is arrived for the new-unseen test set.
# - Read in the Model list object
# - Read in the validation set object (This is now called Data).
# - Read in the feedback data (This is called Feedback).
# - Make the Feedback data into a split of training and testing (90 - 10 split)
# - Assume that the feedback training data is 60% of the data, take out the remaining 40% from the Data. This becomes the training data
# - Train a model on this data.
# - Validate this model on the feedback testing set
# - Validate the Model list on the feedback testing set
# - Then, take top x % of trees from the new model. Drop x % of trees from the old model
# - Print the overall performance of this combined two models on feedback testing set.
# - Apply the model on the new-unseen test set
# - Save the model object as List[[obj_1, trees-to-take], [obj_2, trees-to-take], .... [obj_iter, trees-to-take]]
# - Save the validation set for next stage
# - Repeat the iterations whenever there is a feedback.

# # functions - for tweaking # #
# ownStrip - strips a vector of "t1-100" to return 100
ownStrip = function(treeIDs) {
	return(as.numeric(unlist(strsplit(treeIDs, "-"))[seq(2, length(treeIDs)*2, 2)]))
}

setwd("C:\\Users\\ka294056\\Desktop\\IncrementalLearning")

# # read in the necessary objects into environment # #
# 1) random forest model list object
# 2) validation data set
load("objects//fit.rda")
load("objects//tree.rda")
load("objects//validation.rda")
load("objects//iterCount.rda")

# # get the existing tree numbers
totalTrees = do.call("c", treeList)
totalTrees = unlist(strsplit(totalTrees, "-"))[seq(1, (length(totalTrees) * 2), 2)]
totalTrees = as.numeric(gsub("t", "", unique(totalTrees)))

data = validation
rm(validation)

# # read the feedback data and the new-unseen test data # #
source("read-values.R")
fbSrc = paste0("input-data//", inputParameters$feedback)
testSrc = paste0("input-data//", inputParameters$test)

# # read the other necessary parameters # #
# dropPerc = % of trees to be replaced
# splitPerc = % of the feedback data to be mixed with original data
# test = unseen test set
dropPerc = as.numeric(inputParameters$dropPerc)
splitPerc = as.numeric(inputParameters$splitPerc)
cutoff = as.numeric(inputParameters$cutoff)
feedback = read.csv(fbSrc)
test = read.csv(testSrc)

# # split the feedback data into feedbackTrain and feedbackTest (90 - 10 split) # #
library(caret)
index = createDataPartition(feedback$Y, times = 1, p = 0.9, list = FALSE)
feedbackTrain = feedback[index, ]
feedbackTest = feedback[-index, ]

# # make the full-fledged training data using feedbackTrain and validation object data # #
dataNRows = round(nrow(feedbackTrain)/splitPerc)
validationNRows = dataNRows - nrow(feedbackTrain)
train = rbind(feedbackTrain, data[sample(rownames(data), validationNRows), ])

# # fit a random forest model on the train # #
library(randomForest)
fit = randomForest(factor(Y) ~ ., data = train, ntree = 400)

# # evaluate this fit and the model list object fit on the feedbackTest set **[tree-wise]** # #
# 2) evaluate the model list object fit **[tree-wise]**
p1 = list()
for(eachFit in 1:length(modelList)) {
	p1[[eachFit]] = predict(modelList[[eachFit]], feedbackTest, predict.all = TRUE)$individual[, ownStrip(treeList[[eachFit]])]
}
# -- store the names of the trees to name it for p1 below
tNames = vector()
for(eachFit in 1:length(modelList)) {
	tNames = c(tNames, treeList[[eachFit]])
}

p1 = data.frame(do.call("cbind", p1))
colnames(p1) = tNames
# 2) evaluate the latest fit **[tree-wise]**
p2 = predict(fit, feedbackTest, predict.all = TRUE)$individual
# 3) evaluate
getPerformance = function(treeVotes, original) {
	treeVotes = sapply(treeVotes, as.numeric)
	return(performance(original = original, predicted = treeVotes)$fscore)
}
p1FScores = apply(p1, 2, getPerformance, original = feedbackTest$Y)
p2FScores = apply(p2, 2, getPerformance, original = feedbackTest$Y)

# # drop x % trees in p1 by using p1FScores and take top x% trees in p2 by using p2FScores # #
p1Take = colnames(p1)[order(p1FScores, decreasing = TRUE)][1:(length(p1FScores) - (length(p1FScores) * dropPerc))]
p1Take = sort(p1Take)
# sort p1Take by trees (each tree is an item in list)
trees = unlist(strsplit(p1Take, "-"))[seq(1, (length(p1Take) * 2), 2)]
values = unlist(strsplit(p1Take, "-"))[seq(2, (length(p1Take) * 2), 2)]
p1Take = list()
ct = 1
for(i in unique(trees)) {
	p1Take[[ct]] = paste(i, values[which(trees == i)], sep = "-")
	ct = ct + 1
}
p2Take = order(p2FScores, decreasing = TRUE)[1:(length(p2FScores) * dropPerc)]
# add tree number as prefix
p2Take = paste(paste0("t", (iterCount + 1)), p2Take, sep = "-")


# # Saving objects # #
# - Note: Store old models for performance calculation in the step below
# - 1) Save the model object as List [[obj_1, obj_2, .... obj_iter]]
# - 2) Save the trees to take object as List [[trees for obj_1, trees for obj_2, ... trees for obj_iter]]
# - 3) Save the validation set for next stage (here feedbackTest)
modelListOld = modelList
treeListOld = treeList

# # drop the models that has no trees
present = as.numeric(gsub("t", "", unique(trees)))
present = which(totalTrees %in% present)
absent = setdiff(1:length(modelList), present)
modelList[absent] = NULL

# # add the new model fit and new trees-to-take to the existing and then save
modelList = c(modelList, list(fit))
treeList = c(p1Take, list(p2Take))
save(modelList, file = "objects//fit.rda")
save(treeList, file = "objects//tree.rda")
load("objects//validation.rda")
validation = rbind(validation, feedbackTest)
save(validation, file = "objects//validation.rda")
iterCount = iterCount + 1
save(iterCount, file = "objects//iterCount.rda")

# # - Print the overall performance of the old model on feedback testing set. # #
# # - Also, Print the overall performance of this combined two models on feedback testing set. # #
# -- for old model --
pOld = list()
for(eachFit in 1:length(modelListOld)) {
	pOld[[eachFit]] = predict(modelListOld[[eachFit]], feedbackTest, predict.all = TRUE)$individual[, ownStrip(treeListOld[[eachFit]])]
}
pOld = do.call("cbind", pOld)
	# do max voting
	pOld = as.numeric(apply(pOld, 1, function(x) sum(as.numeric(x))/length(x)) >= cutoff)

# -- for new model --
pNew = list()
for(eachFit in 1:length(modelList)) {
	pNew[[eachFit]] = predict(modelList[[eachFit]], feedbackTest, predict.all = TRUE)$individual[, ownStrip(treeList[[eachFit]])]
}
pNew = do.call("cbind", pNew)
	# do max voting
	pNew = as.numeric(apply(pNew, 1, function(x) sum(as.numeric(x))/length(x)) >= cutoff)

# # to make the (above process) new model prediction east - we wrote a function # #
newModelPredict = function(modelList, treeList, data) {
	pNew = list()
	for(eachFit in 1:length(modelList)) {
		pNew[[eachFit]] = predict(modelList[[eachFit]], data, predict.all = TRUE)$individual[, ownStrip(treeList[[eachFit]])]
	}
	pNew = do.call("cbind", pNew)
		# do max voting
		pNew = as.numeric(apply(pNew, 1, function(x) sum(as.numeric(x))/length(x)) >= cutoff)
	return(pNew)
}

# # printing performances in file # #
# # 1) old model on feedback test set
# # 2) new model on feedback test set
# # 3) new on unseen test set
# To get performance function that calculate precision recall and fscore
source("https://raw.githubusercontent.com/kartheekpnsn/machine-learning-codes/master/R/functions.R")
sink("logs//incremental-run.txt", append = T)
print("# # # # # # # # # # INCREMENTAL RUN # # # # # # # # # # # #")
print("# # # # # # # # # # OLD MODEL ON FEEDBACK TEST SET # # # # # # # # # #")
print(paste0("On - ", date()))
print(performance(original = feedbackTest$Y, predicted = pOld)[, 1:4])
print("# # # # # # # # # # NEW MODEL ON FEEDBACK TEST SET # # # # # # # # # #")
print(performance(original = feedbackTest$Y, predicted = pNew)[, 1:4])
print("# # # # # # # # # # OLD MODEL ON UNSEEN TEST SET # # # # # # # # # #")
print(performance(original = test$Y, predicted = newModelPredict(modelListOld, treeListOld, data = test))[, 1:4])
print("# # # # # # # # # # NEW MODEL ON UNSEEN TEST SET # # # # # # # # # #")
print(performance(original = test$Y, predicted = newModelPredict(modelList, treeList, data = test))[, 1:4])
print("# # # # # # # # # # # # # # # # # # # # # # # # # # # #")
sink()
