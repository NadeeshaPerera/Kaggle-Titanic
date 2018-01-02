######################################################################################################
# Name : Nadeesha Perera
# Date : 12/18/2017
# Topic : Titanic data set - Kaggle - Submission 2
# Purpose : Using Random forest - attempt 1
#####################################################################################################

rm(list = ls(all = TRUE))

# Load the required libraries
library(data.table)
library(party)
library(randomForest)
library(stringr)


train <- fread("train.csv")
test <- fread("test.csv")

head(train)

title <- rep(0, nrow(train))

extract_Title <- function(a){
  if(length(grep("Miss.", a))>0){
    return("Miss.")
  }
  
  else if(length(grep("Mrs.", a)) > 0){
    return("Mrs.")
  }
  
  else if(length(grep("Master.", a)) > 0){
    return("Master.")
  }
  
  else if(length(grep("Mr.", a)) > 0){
    return("Mr.")
  }
  
  else{
    return("Other")
    }
}

for(i in 1:nrow(train)){
  title[i] <- extract_Title(train$Name[i])
}

train$Title <- as.factor(title)

str(train)
table(train$Title)

train$familysize <- train$SibSp+train$Parch+1


#model1

rf.train.1 <- train[c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, ntree = 1000, importance = TRUE)
rf.1
varImpPlot(rf.1)

#model2

rf.train.2 <- train[c("Pclass", "Title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, ntree = 1000, importance = TRUE)
rf.2

#model3

rf.train.3 <- train[c("Pclass", "Title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, ntree = 1000, importance = TRUE)
rf.3


#model4

rf.train.4 <- train[c("Pclass", "Title", "Parch", "SibSp")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, ntree = 1000, importance = TRUE)
rf.4

varImpPlot(rf.4)

#model5

rf.train.5 <- train[c("Pclass", "Title", "familysize")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

#model6

rf.train.6 <- train[c("Pclass", "Title", "familysize", "SibSp")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)


#model7

rf.train.7 <- train[c("Pclass", "Title", "familysize", "Parch")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)

# model 5 gives the lowest error rate thus far

# Preparing test date set

title_test <- rep(0, nrow(test))

for(i in 1:nrow(test)){
  title_test[i] <- extract_Title(test$Name[i])
  
}

test$Title <- as.factor(title_test)

test$familysize <- 1+test$SibSp+test$Parch

rf.test.5 <- test[c("Pclass", "Title", "familysize")]

pred <- predict(rf.5, newdata = rf.test.5)
table(pred)

test$Survived <- pred
submission_v2 <- test[c("PassengerId", "Survived")]

###########################################################################################
# write data to a file

write.csv(submission_v2, file = "Submission_V2.csv", row.names = FALSE)



