library(MASS, quietly=TRUE)
library(caret)
library(ROSE)
library(ROCR)
library(BBmisc)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(ROSE)
library(lattice)
library(ggplot2)
library(caret)

getwd()
set.seed(123)
#loading training and testing dataset
NSP_train <- read.csv(file = "train.csv", stringsAsFactors = FALSE)
NSP_test <- read.csv(file = "test.csv", stringsAsFactors = FALSE)


dim(NSP_train)

dim(NSP_test)
str(NSP_train)
#check that no datapoint is missing, otherwise we need to fix the dataset
apply(NSP_train,2,function(x) sum(is.na(x)))
apply(NSP_test,2,function(x) sum(is.na(x)))

#to deal with missing values let first combine test and train dataset
#first we add a new column in both test and train, named "isTrainData"
#isTrainData =TRUE for all row in Train dataset and, =FALSE for all row in Test dataset
NSP_train$isTrainData <- TRUE
NSP_test$isTrainData <- FALSE

NSP_test$NSP <- NA

names(NSP_train)
names(NSP_test)

NSP_test <- NSP_test[,c((1:21),23,22)]

NSP_full <- rbind(NSP_train, NSP_test)


str(NSP_full)




#now extract back Train and Test dataset from full dataset
#to identify train and test data we use "isTrainData" variable value
NSP_train <- NSP_full[NSP_full$isTrainData==TRUE, ]
NSP_test <- NSP_full[NSP_full$isTrainData==FALSE, ]

NSP_train$NSP <- as.factor(NSP_train$NSP)
head(NSP_test)
#fitting a random forest model
library(randomForest)
#now first defined formula for predictiion of "Survived"
NSP_equation <- "NSP~LB+AC+FM+UC+DL+DS+DP+ASTV+MSTV+ALTV+MLTV+Width+Min+Max+Nmax+Nzeros+Mode+Mean+Median+Variance+Tendency"

NSP_formula <- as.formula(NSP_equation)
NSP_model <- randomForest(formula= NSP_formula, 
                          data= NSP_train, 
                          ntree = 1150, 
                          mtry = 15, 
                          nodesize = 1.655, #0.001*nrow(NSP_train),
                          do.trace=T,
                          norm.votes  = FALSE,
                          proximity=TRUE)
plot(NSP_model)



mean(NSP_model$err.rate)
attributes(NSP_model)
#prediction on test dataset
View(NSP_test)
NSP <- predict(NSP_model, newdata = NSP_test)


pred_df <- as.data.frame(NSP)
pred_df$Patient <- c(1:891)
pred_df <- pred_df[,c(2,1)]
View(pred_df)
row.names(pred_df) <- NULL
write.csv(pred_df,'solution.csv',row.names = FALSE)
