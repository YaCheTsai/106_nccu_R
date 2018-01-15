library(plyr)
library(dplyr)
library(randomForest)

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_YaCheTsai.R input ", call.=FALSE)
} 

outputfile <- args[4]



data <- read.csv("Archaeal_tfpssm.csv", header = F)
#glimpse(data)
data <- data[,-1]


levels(data[,1])
head(data[,5599:5602]) 
test <-0
calibration<-0
training<-0
k = as.integer(args[2])

data[[5602]] <- sample(1:k, nrow(data), replace = TRUE)

list <- 1:k

prediction <- data.frame()
testsetCopy <- data.frame()
prediction1 <- data.frame()
testsetCopy1 <- data.frame()
prediction2 <- data.frame()
testsetCopy2 <- data.frame()

for(i in c(1:k)){
  

  
  trainingset <- subset(data, data[[5602]] %in% list[-(i:(i+1)%%k)])
  
  testset <- subset(data, data[[5602]] %in% c(i))
  
  calibrationset <- subset(data, data[[5602]] %in% c((i+1)%%k))

 
  mymodel <- randomForest(trainingset$V2 ~ ., data = trainingset, ntree = 100)

  
  temp <- as.data.frame(predict(mymodel, testset[,-1]))
  temp1 <- as.data.frame(predict(mymodel, trainingset[,-1]))
  temp2 <- as.data.frame(predict(mymodel, calibrationset[,-1]))
  

  
  prediction <- rbind(prediction, temp)
  prediction1 <- rbind(prediction1, temp1)
  prediction2 <- rbind(prediction2, temp2)
  

  
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  testsetCopy1 <- rbind(testsetCopy1, as.data.frame(trainingset[,1]))
  testsetCopy2 <- rbind(testsetCopy2, as.data.frame(calibrationset[,1]))
  

  
  result <- cbind(prediction, testsetCopy[, 1])
  names(result) <- c("Predicted", "Actual")
  result1 <- subset(result, result$Actual == result$Predicted)
  test <- test +(nrow(result1)/nrow(result))
  
  resulta <- cbind(prediction1, testsetCopy1[, 1])
  names(resulta) <- c("Predicted", "Actual")
  result1a <- subset(resulta, resulta$Actual == resulta$Predicted)
  training <- training +(nrow(result1a)/nrow(resulta))
  
  resultb <- cbind(prediction2, testsetCopy2[, 1])
  names(resultb) <- c("Predicted", "Actual")
  result1b <- subset(resultb, resultb$Actual == resultb$Predicted)
  calibration <- calibration +(nrow(result1b)/nrow(resultb))
 
  
}

set <- c("trainning", "calibration", "test")
accuracy <- c(training/k,calibration/k, test/k)

output <- data.frame(set, accuracy)
write.table(output, file = outputfile, sep = ",", row.names = FALSE)


