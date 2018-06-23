

install.packages("caret")
install.packages("knitr")
install.packages("randomForest")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("corrplot")
install.packages("e1071")
install.packages("contrib.url")
install.packages("rmarkdown")

library(caret)
library(knitr)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(corrplot)
library(e1071)
library(rmarkdown)

# Download the dataset 
trainingData <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingData  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"


# Load the dataset into memory
trainingData <- read.csv(url(trainUrl), na.strings = c("NA", "#DIV/0!", ""))
testingData <- read.csv(url(testUrl), na.strings = c("NA", "#DIV/0!", ""))

#
trainingData <- trainingData[, colSums(is.na(trainingData)) == 0]
testingData <- testingData[, colSums(is.na(testingData)) == 0]

# Delete variables that are not related 
trainingData <- trainingData[, -c(1:7)]
testingData <- testingData[, -c(1:7)]

# partioning the training set into two different dataset

traningPartitionData <- createDataPartition(trainingData$classe,  p = 0.7, list = F)
trainingDataSet <- trainingData[traningPartitionData, ]
testingDataSet <- trainingData[-traningPartitionData, ]
dim(trainingData); dim(testingDataSet)


# Prediction model 1 - decision tree
decisionTreeModel <- rpart(classe ~ ., data = trainingDataSet, method = "class")
decisionTreePrediction <- predict(decisionTreeModel, testingDataSet, type = "class")

# Plot Decision Tree
rpart.plot(decisionTreeModel, main = "Decision Tree", under = T, faclen = 0)


# Prediction model 2 - random forest
randomForestModel <- randomForest(classe ~. , data = trainingDataSet, method = "class")
randomForestPrediction <- predict(randomForestModel, testingDataSet, type = "class")

confusionMatrix(randomForestPrediction, testingDataSet$classe)


