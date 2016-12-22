##################################################################################
######################## Stage - 1: Go through read-me.txt #######################
##################################################################################

# Stage:1
# -------
# - Take the labelled data (call it Data), divide it into train and validation sets (75-25 split)
# - Use the train data to build the model
# - Print the performance of the model on the validation set
# - Apply the model on the new-unseen test set.
# - Save the model object as List [[obj_1, obj_2]]
# - Save the trees to take object as List [[trees for obj_1, trees for obj_2]]
# - Save the validation set for next stage
# - Sit back and wait for the feedback


setwd("C:\\Users\\ka294056\\Desktop\\IncrementalLearning")

# # Create necessary directories # #
# 1) LOGS
# 2) Objects
dir.create("logs", showWarnings = F)
dir.create("objects", showWarnings = F)

source("read-values.R")

dataSrc = paste0("input-data//", inputParameters$train)
testSrc = paste0("input-data//", inputParameters$test)
data = read.csv(dataSrc)
test = read.csv(testSrc)

# # divide train and test sets (75-25 split) # #
library(caret)
index = createDataPartition(data$Y, p = 0.75, times = 1, list = FALSE)
train = data[index, ]
validation = data[-index, ]

# # fit a basic random forest model # #
library(randomForest)
fit = randomForest(factor(Y) ~ ., data = train, ntree = 400)

# # print performance on validation set # #
# To get performance function that calculate precision recall and fscore
source("https://raw.githubusercontent.com/kartheekpnsn/machine-learning-codes/master/R/functions.R")
sink("logs//initial-run.txt", append = T)
print("# # # # # # # # # # INITIAL RUN # # # # # # # # # # # #")
print("# # # # # # # # # # VALIDATION  SET # # # # # # # # # #")
print(paste0("On - ", date()))
print(performance(original = validation$Y, predicted = predict(fit, validation))[, 1:4])
print("# # # # # # # # # # ON TEST SET # # # # # # # # # # # #")
print(performance(original = test$Y, predicted = predict(fit, test))[, 1:4])
print("# # # # # # # # # # # # # # # # # # # # # # # # # # # #")
sink()

# # save the necessary objects # #
# 1) model object as list in the form of [[obj_1, obj_2]]
# 2) Save the trees to take object as List [[trees for obj_1, trees for obj_2]]
# 3) validation set
modelList = list(fit)
# add tree number as prefix
treeList = list(paste("t1", 1:400, sep = "-"))
# save iterCount as 1
iterCount = 1
save(modelList, file = "objects//fit.rda")
save(treeList, file = "objects//tree.rda")
save(validation, file = "objects//validation.rda")
save(iterCount, file = "objects//iterCount.rda")