#install.packages('readr')
#install.packages('randomForest')
#install.packages('caret')

library(readr)
library(randomForest)
library(caret)


train <- read_csv("Dateset/train.csv")
test <- read_csv("Dateset/test.csv")

str(train)

train_factor <- train
train_factor$weather <- factor(train$weather)
train_factor$holiday <- factor(train$holiday)
train_factor$workingday <- factor(train$workingday)
train_factor$season <- factor(train$season)

test_factor <- test
test_factor$weather <- factor(test$weather)
test_factor$holiday <- factor(test$holiday)
test_factor$workingday <- factor(test$workingday)
test_factor$season <- factor(test$season)

train_factor$time <- substring(train$datetime,12,20)
test_factor$time <- substring(test$datetime,12,20)

train_factor$time <- factor(train_factor$time)
test_factor$time <- factor(test_factor$time)

train_factor$day <- weekdays(as.Date(train_factor$datetime))
train_factor$day <- as.factor(train_factor$day)
test_factor$day <- weekdays(as.Date(test_factor$datetime))
test_factor$day <- as.factor(test_factor$day)

aggregate(train_factor[,"count"],list(train_factor$day),mean)

#create Sunday variable
train_factor$sunday[train_factor$day == "星期日"] <- "1"
train_factor$sunday[train_factor$day != "星期日"] <- "0"

test_factor$sunday[test_factor$day == "星期日"] <- "1"
test_factor$sunday[test_factor$day != "星期日"] <- "0"

#convert to factor
train_factor$sunday <- as.factor(train_factor$sunday)
test_factor$sunday <- as.factor(test_factor$sunday)

#convert time and create $hour as integer to evaluate
train_factor$hour<- as.numeric(substr(train_factor$time,1,2))
test_factor$hour<- as.numeric(substr(test_factor$time,1,2))


#4AM - 9AM = 1
train_factor$daypart[(train_factor$hour < 10) & (train_factor$hour > 3)] <- 1
test_factor$daypart[(test_factor$hour < 10) & (test_factor$hour > 3)] <- 1


#10AM - 3PM = 2
train_factor$daypart[(train_factor$hour < 16) & (train_factor$hour > 9)] <- 2
test_factor$daypart[(test_factor$hour < 16) & (test_factor$hour > 9)] <- 2


#4PM - 9PM = 3
train_factor$daypart[(train_factor$hour < 22) & (train_factor$hour > 15)] <- 3
test_factor$daypart[(test_factor$hour < 22) & (test_factor$hour > 15)] <- 3

train_factor[["daypart"]][is.na(train_factor[["daypart"]])] <- 0
test_factor[["daypart"]][is.na(test_factor[["daypart"]])] <- 0

#convert daypart to factor
train_factor$daypart <- as.factor(train_factor$daypart)
test_factor$daypart <- as.factor(test_factor$daypart)


train_factor$hour <- as.factor(train_factor$hour)
test_factor$hour <- as.factor(test_factor$hour)


print (train_factor)
print (test_factor)


formula <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart + sunday
folds <- cut(seq(1,nrow(train_factor)),breaks=5,labels=FALSE)
prediction <- data.frame()
testsetCopy<- data.frame()
Pnum <- 0
k = 5
for(i in 1:k){
  
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- train_factor[testIndexes, ]
  trainData <- train_factor[-testIndexes, ]
  
  fit <- randomForest(formula, data=trainData, importance=TRUE, ntree=500, mtry = 6)
  
  temp <- as.data.frame(predict(fit, testData[,-12]))
  
  prediction <- rbind(prediction, temp)
  
  testsetCopy <- rbind(testsetCopy, as.data.frame(testData[,12]))
  
  result <- cbind(prediction, testsetCopy[, 1])
  
  names(result) <- c("Predicted", "Actual")
  
  R2<-caret::RMSE(result$Predicted,result$Actual)
  print(R2)
  if(Pnum < R2){
    Pnum <- R2
    index <- fit
  }
  
}


Prediction <- predict(fit, test_factor)


submit <- data.frame(datetime = test_factor$datetime, count = Prediction)

write.csv(submit, file="Submission.csv",row.names=FALSE)

