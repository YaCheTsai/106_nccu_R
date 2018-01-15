library(plyr)
library(dplyr)
library(readr)
library(randomForest)
library(rpart)

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_106753028.R input ", call.=FALSE)
} 

outputfile <- args[4]

train <- read_csv("Titanic_Data/train.csv")
test <- read_csv("Titanic_Data/test.csv")
#列出第一筆Name
k = as.integer(args[2])
train$Name[1]


#將test的Survived設為NA

test$Survived <- NA

#把train和test資料合併成一個新的資料combi

combi <- rbind(train, test)

#把Name從factor轉換成character

combi$Name <-as.character(combi$Name)

#轉換後執行列出第一筆變成這樣

combi$Name[1]


#用strsplit把Name拆開

strsplit(combi$Name[1], split= '[,.]')[[1]]


#若要把第二個值分離出來則指令如下

strsplit(combi$Name[1], split= '[,.]')[[1]][2]


#用sapply把所有Name裡的title extra出到一個新的欄位Title

combi$Title <- sapply(combi$Name, FUN=function(x){strsplit(x, split='[,.]')[[1]][2]})

#把Title裡的空白消除掉


combi$Title <- sub(" ", "", combi$Title)

#將所有的Title簡化為Mlle, Sir, Lady三個類型

combi$Title[combi$Title %in% c("Mme", "Mlle")] <- "Mlle"
combi$Title[combi$Title %in% c("Capt", "Don", "Major", "Sir")] <-"Sir"
combi$Title[combi$Title %in% c("Dona", "Lady", "the Countess", "Jonkheer")] <- "Lady"

#將combi$Title從character轉換成factor

combi$Title <-factor(combi$Title)

#新增一個欄位FamilySize, 將同行人數的兄弟姐妹配偶父母小孩及本人加總

combi$FamilySize <- combi$SibSp + combi$Parch +1

#從Name裡extra出姓

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

#新增FamilyID欄位，將FamilySize及姓結合在一起，這樣就可以知道哪些人是同一家人

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

#將2人及以下人數，非家族旅行的全部稱做Small, 以減少資料level

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'


#將FamilyID及人數匯出成famIDs

famIDs <-data.frame(table(combi$FamilyID))

#將Freq小於2次的資料匯出

famIDs <- famIDs[famIDs$Freq <= 2,]

#將上述選出來的名單的FamilyID都改成Small

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'


combi$FamilyID <- factor(combi$FamilyID)

#將combi資料再寫回train和test

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = combi[!is.na(combi$Age),], method="anova")

#利用上述建立的模型Agefit去預測Age為NA的資料

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

#Embarked有2筆空白資料
                
summary(combi$Embarked)
               
                
#用which找出哪2筆是空白資料
                
which(combi$Embarked == '')
               
                
#將這2筆資料填入S
                
combi$Embarked[c(62,830)] = "S"
                
#再把Embarked轉換為factor
                
combi$Embarked <- factor(combi$Embarked)
                
#Fare裡有一筆空白資料
                
summary(combi$Fare)
                
#找出Fare裡哪筆是na
                
which(is.na(combi$Fare))

                
#將這筆Fare填入Fare的中位數
                
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
                
#為符合random forest對level的要求，將FamilySize裡對Samll的標準拉高到3個人以下
                
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

combi$Sex[combi$Sex == "male"] <- 1
combi$Sex[combi$Sex <= "female"] <- 0
combi$Sex <- as.factor(combi$Sex)              
#將combi再拆成train及test
        
train <-combi[1:891,-11 ]
test <-combi[892:1309,-11]


#加入上述欄位再一次產生dTree預測模型

#fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data=train, method = "class")

#將預測資料寫入submit以供上傳


#Create 10 equally size folds
folds <- cut(seq(1,nrow(train)),breaks=10,labels=FALSE)

prediction <- data.frame()
testsetCopy<- data.frame()

#Perform 10 fold cross validation

print(train)
Pnum <- 0

for(i in 1:k){
 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- train[testIndexes, ]
  trainData <- train[-testIndexes, ]
  
  fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +FamilyID2, data=trainData, importance=TRUE, ntree=2000)
  
  Prediction <- predict(fit, testData)
  
  
  temp <- as.data.frame(predict(fit, testData[,-2]))
  
  prediction <- rbind(prediction, temp)
  
  testsetCopy <- rbind(testsetCopy, as.data.frame(testData[,2]))
  
  result <- cbind(prediction, testsetCopy[, 1])
  
  names(result) <- c("Predicted", "Actual")
  
  result1 <- subset(result, result$Actual == result$Predicted)
  num <- (nrow(result1)/nrow(result))
  
  if(Pnum < num){
    Pnum <- (nrow(result1)/nrow(result))
    index <- fit
  }
  
  
}
Prediction <- predict(index, test)

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)


#將submit資料寫入檔案，不包含row.names

write.csv(submit , file=outputfile, row.names = FALSE)

