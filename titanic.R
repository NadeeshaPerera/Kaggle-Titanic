######################################################################################################
# Name : Nadeesha Perera
# Date : 12/18/2017
# Topic : Titanic data set - Kaggle
#####################################################################################################

# Clear work space
rm(list = ls(all = TRUE))

# Load the required libraries
library(data.table)
library(stringr)

# Read files to vairables
train <- fread("train.csv")
test <- fread("test.csv")

# Look at structure of the data
str(train)
str(test)

# Look at summaries of the data
summary(train)
summary(test)

# Convert the "Sex" variable from character to numeric
train$Sex <- ifelse(train$Sex == "male", 0, 1)
test$Sex <- ifelse(test$Sex == "male", 0, 1)

View(train)


# function to extract title

extract_title <- function(name){
    if(length(grep("Miss.", name)) > 0){
      return("Miss.")
    }
    else if(length(grep("Mrs.", name))>0){
      return("Mrs.")
    }
    else if(length(grep("Master.", name))>0){
      return("Master.")
    }
    else if(length(grep("Mr.", name))>0){
      return("Mr.")
    }
    else{
      return("Other")
    }
}


name_title <- c(rep(0, nrow(train)))

for(i in 1:nrow(train)){
  name_title[i] <- extract_title(train$Name[i])
}

train$Title <- as.factor(name_title)

View(train)

table(train$Title)
str(train)

###################### miss data #########################################

# Selecting data sets based on various age and sex attributes

misses <- train[which(train$Title == "Miss.")]
summary(misses)

misses_V1 <- misses[!is.na(misses$Age)]
summary(misses_V1)

model_misses <- glm(Survived~Pclass+Age+SibSp+Parch, family = binomial(link = "logit"), data = misses_V1)

## EM-Algorithm for missing data
misses_V2 <- misses
Xdata_miss <- cbind(misses$Pclass, misses$SibSp, misses$Parch)

for(i in 1:100){
  model1_miss <- glm(Survived~Xdata_miss+Age, family = binomial(link = "logit"), data = misses_V2)
  bminus_miss <- coef(model1_miss)[1:4]
  b_miss      <- coef(model1_miss)[5]
  pred_miss   <- (log(misses$Survived/(1-misses$Survived)) - cbind(rep(1, nrow(misses)), Xdata_miss) %*% bminus_miss)/b_miss
  pred_miss   <- ifelse(pred_miss >= 0.75, pred_miss, 0.75)
  pred_miss   <- ifelse(pred_miss <= 63, pred_miss, 63)
  pred_miss   <- ifelse(is.na(misses$Age), pred_miss, misses$Age)
  misses_V2$Age <- pred_miss
}



###################### Mr. data #########################################

# Selecting data sets based on various age and sex attributes

mr <- train[which(train$Title == "Mr.")]
summary(mr)

mr_V1 <- mr[!is.na(mr$Age)]
summary(mr_V1)

model_mr <- glm(Survived~Pclass+Age+SibSp+Parch, family = binomial(link = "logit"), data = mr_V1)


## EM-Algorithm for missing data
mr_V2 <- mr
Xdata_mr <- cbind(mr$Pclass, mr$SibSp, mr$Parch)

for(i in 1:100){
  model1_mr <- glm(Survived~Xdata_mr+Age, family = binomial(link = "logit"), data = mr_V2)
  bminus_mr <- coef(model1_mr)[1:4]
  b_mr      <- coef(model1_mr)[5]
  pred_mr   <- (log(mr$Survived/(1-mr$Survived)) - cbind(rep(1, nrow(mr)), Xdata_mr) %*% bminus_mr)/b_mr
  pred_mr   <- ifelse(pred_mr >= 11, pred_mr, 11)
  pred_mr   <- ifelse(pred_mr <= 80, pred_mr, 80)
  pred_mr   <- ifelse(is.na(mr$Age), pred_mr, mr$Age)
  mr_V2$Age <- pred_mr
}

summary(mr_V2)



###################### Mrs. data #########################################

# Selecting data sets based on various age and sex attributes

mrs <- train[which(train$Title == "Mrs.")]
summary(mrs)

mrs_V1 <- mrs[!is.na(mrs$Age)]
summary(mrs_V1)

model_mrs <- glm(Survived~Pclass+Age+SibSp+Parch, family = binomial(link = "logit"), data = mrs_V1)


## EM-Algorithm for missing data
mrs_V2 <- mrs
Xdata_mrs <- cbind(mrs$Pclass, mrs$SibSp, mrs$Parch)

for(i in 1:100){
  model1_mrs <- glm(Survived~Xdata_mrs+Age, family = binomial(link = "logit"), data = mrs_V2)
  bminus_mrs <- coef(model1_mrs)[1:4]
  b_mrs      <- coef(model1_mrs)[5]
  pred_mrs   <- (log(mrs$Survived/(1-mrs$Survived)) - cbind(rep(1, nrow(mrs)), Xdata_mrs) %*% bminus_mrs)/b_mrs
  pred_mrs   <- ifelse(pred_mrs >= 14, pred_mrs, 14)
  pred_mrs   <- ifelse(pred_mrs <= 63, pred_mrs, 63)
  pred_mrs   <- ifelse(is.na(mrs$Age), pred_mrs, mrs$Age)
  mrs_V2$Age <- pred_mrs
}

summary(mrs_V2)


###################### Master. data #########################################

# Selecting data sets based on various age and sex attributes

master <- train[which(train$Title == "Master.")]
summary(master)

master_V1 <- master[!is.na(master$Age)]
summary(master_V1)

model_master <- glm(Survived~Pclass+Age+SibSp+Parch, family = binomial(link = "logit"), data = master_V1)


## EM-Algorithm for missing data
master_V2 <- master
Xdata_master <- cbind(master$Pclass, master$SibSp, master$Parch)

for(i in 1:100){
  model1_master <- glm(Survived~Xdata_master+Age, family = binomial(link = "logit"), data = master_V2)
  bminus_master <- coef(model1_master)[1:4]
  b_master      <- coef(model1_master)[5]
  pred_master   <- (log(master$Survived/(1-master$Survived)) - cbind(rep(1, nrow(master)), Xdata_master) %*% bminus_master)/b_master
  pred_master   <- ifelse(pred_master >= 0.42, pred_master, 0.42)
  pred_master   <- ifelse(pred_master <= 12, pred_master, 12)
  pred_master   <- ifelse(is.na(master$Age), pred_master, master$Age)
  master_V2$Age <- pred_master
}

summary(master_V2)




###################### Other data #########################################

# Selecting data sets based on various age and sex attributes

other <- train[which(train$Title == "Other")]
summary(other)

other_V1 <- other[!is.na(other$Age)]
summary(other_V1)

model_other <- glm(Survived~Pclass+Age+SibSp+Parch, family = binomial(link = "logit"), data = other_V1)


## EM-Algorithm for missing data
other_V2 <- other
Xdata_other <- cbind(other$Pclass, other$SibSp, other$Parch)

for(i in 1:100){
  model1_other <- glm(Survived~Xdata_other+Age, family = binomial(link = "logit"), data = other_V2)
  bminus_other <- coef(model1_other)[1:4]
  b_other      <- coef(model1_other)[5]
  pred_other   <- (log(other$Survived/(1-other$Survived)) - cbind(rep(1, nrow(other)), Xdata_other) %*% bminus_other)/b_other
  pred_other   <- ifelse(pred_other >= 23, pred_other, 23)
  pred_other   <- ifelse(pred_other <= 70, pred_other, 70)
  pred_other   <- ifelse(is.na(other$Age), pred_other, other$Age)
  other_V2$Age <- pred_other
}

summary(other_V2)




# Combining all the data together

total <- rbind(misses_V2, mr_V2, mrs_V2, master_V2, other_V2)

summary(total)
str(total)
View(total)

###################################################################################################
# Create model

model_total <- glm(Survived~Pclass+Sex+Age+SibSp+Parch, family = binomial(link = "logit"), data = total)

# Stepwise regression to find optimum BIC model

model_final <- step(model_total, direction = "backward", k = log(nrow(total)))

summary(model_final)

####################################################################################################
# Treat missing values in test data set

summary(test)

test_title <- c(rep(0, nrow(test)))

for(i in 1:nrow(test)){
  test_title[i] <- extract_title(test$Name[i])
}

test$Title <- test_title

###############################################################################################
# Finding missing values for age in test data file.
# Find categories in test data that have missing values
# Replace them with the average ages of the corresponding categories in train data


testNA <- test[is.na(test$Age)]
table(testNA$Pclass, testNA$Title, testNA$SibSp)

# context1 = title: master, pClass: 3, SibSp: 0

context1_a <- total[which(total$Title == "Master.")]
context1_b <- context1_a[which(context1_a$Pclass == 3)]
context1_c <- context1_b[which(context1_b$SibSp == 0)]

summary(context1_c)
mean(context1_c$Age)

# context2 = title: miss, pClass: 3, SibSp: 0

context2_a <- total[which(total$Title == "Miss.")]
context2_b <- context2_a[which(context2_a$Pclass == 3)]
context2_c <- context2_b[which(context2_b$SibSp == 0)]

summary(context2_c)
mean(context2_c$Age)


# context3 = title: mr, pClass: 3, SibSp: 0

context3_a <- total[which(total$Title == "Mr.")]
context3_b <- context3_a[which(context3_a$Pclass == 3)]
context3_c <- context3_b[which(context3_b$SibSp == 0)]

summary(context3_c)
mean(context3_c$Age)


# context4 = title: mrs, pClass: 3, SibSp: 0

context4_a <- total[which(total$Title == "Mrs.")]
context4_b <- context4_a[which(context4_a$Pclass == 3)]
context4_c <- context4_b[which(context4_b$SibSp == 0)]

summary(context4_c)
mean(context4_c$Age)


# context5 = title: other, pClass: 3, SibSp: 0 : Since this category doesn't exist in training data, a different match is used.

context5_a <- total[which(total$Title == "Other")]
context5_b <- context5_a[which(context5_a$Sex == 1)]
context5_c <- context5_b[which(context5_b$SibSp == 0)]

summary(context5_c)
mean(context5_c$Age)



# context6 = title: mr, pClass: 2, SibSp: 0

context6_a <- total[which(total$Title == "Mr.")]
context6_b <- context6_a[which(context6_a$Pclass == 2)]
context6_c <- context6_b[which(context6_b$SibSp == 0)]

summary(context6_c)
mean(context6_c$Age)




# context7 = title: mrs, pClass: 2, SibSp: 0

context7_a <- total[which(total$Title == "Mrs.")]
context7_b <- context7_a[which(context7_a$Pclass == 2)]
context7_c <- context7_b[which(context7_b$SibSp == 0)]

summary(context7_c)
mean(context7_c$Age)



# context8 = title: mr, pClass: 1, SibSp: 0

context8_a <- total[which(total$Title == "Mr.")]
context8_b <- context8_a[which(context8_a$Pclass == 1)]
context8_c <- context8_b[which(context8_b$SibSp == 0)]

summary(context8_c)
mean(context8_c$Age)


# context9 = title: mrs, pClass: 1, SibSp: 0

context9_a <- total[which(total$Title == "Mrs.")]
context9_b <- context9_a[which(context9_a$Pclass == 1)]
context9_c <- context9_b[which(context9_b$SibSp == 0)]

summary(context9_c)
mean(context9_c$Age)


# context10 = title: master, pClass: 3, SibSp: 1

context10_a <- total[which(total$Title == "Master.")]
context10_b <- context10_a[which(context10_a$Pclass == 3)]
context10_c <- context10_b[which(context10_b$SibSp == 1)]

summary(context10_c)
mean(context10_c$Age)


# context11 = title: miss, pClass: 3, SibSp: 1

context11_a <- total[which(total$Title == "Miss.")]
context11_b <- context11_a[which(context11_a$Pclass == 3)]
context11_c <- context11_b[which(context11_b$SibSp == 1)]

summary(context11_c)
mean(context11_c$Age)


# context12 = title: mr, pClass: 3, SibSp: 1

context12_a <- total[which(total$Title == "Mr.")]
context12_b <- context12_a[which(context12_a$Pclass == 3)]
context12_c <- context12_b[which(context12_b$SibSp == 1)]

summary(context12_c)
mean(context12_c$Age)


# context13 = title: mrs, pClass: 3, SibSp: 1

context13_a <- total[which(total$Title == "Mrs.")]
context13_b <- context13_a[which(context13_a$Pclass == 3)]
context13_c <- context13_b[which(context13_b$SibSp == 1)]

summary(context13_c)
mean(context13_c$Age)


# context14 = title: miss, pClass: 3, SibSp: 2

context14_a <- total[which(total$Title == "Miss.")]
context14_b <- context14_a[which(context14_a$Pclass == 3)]
context14_c <- context14_b[which(context14_b$SibSp == 2)]

summary(context14_c)
mean(context14_c$Age)


# context15 = title: mr, pClass: 3, SibSp: 2

context15_a <- total[which(total$Title == "Mr.")]
context15_b <- context15_a[which(context15_a$Pclass == 3)]
context15_c <- context15_b[which(context15_b$SibSp == 2)]

summary(context15_c)
mean(context15_c$Age)


# context16 = title: miss, pClass: 3, SibSp: 8

context16_a <- total[which(total$Title == "Miss.")]
context16_b <- context16_a[which(context16_a$Pclass == 3)]
context16_c <- context16_b[which(context16_b$SibSp == 8)]

summary(context16_c)
mean(context16_c$Age)

###################################################################################################

test_full <- test[!is.na(test$Age)]


context1_1 <- test[which(test$Title == "Master.")] 
context1_2 <- context1_1[which(context1_1$Pclass == 3)]
context1_3 <- context1_2[which(context1_2$SibSp == 0)]
context1 <- context1_3[is.na(context1_3$Age)] 

context1$Age <- mean(context1_c$Age)


context2_1 <- test[which(test$Title == "Miss.")] 
context2_2 <- context2_1[which(context2_1$Pclass == 3)]
context2_3 <- context2_2[which(context2_2$SibSp == 0)]
context2 <- context2_3[is.na(context2_3$Age)] 

context2$Age <- mean(context2_c$Age)
View(context2)


context3_1 <- test[which(test$Title == "Mr.")] 
context3_2 <- context3_1[which(context3_1$Pclass == 3)]
context3_3 <- context3_2[which(context3_2$SibSp == 0)]
context3 <- context3_3[is.na(context3_3$Age)] 

context3$Age <- mean(context3_c$Age)


context4_1 <- test[which(test$Title == "Mrs.")] 
context4_2 <- context4_1[which(context4_1$Pclass == 3)]
context4_3 <- context4_2[which(context4_2$SibSp == 0)]
context4 <- context4_3[is.na(context4_3$Age)] 

context4$Age <- mean(context4_c$Age)


context5_1 <- test[which(test$Title == "Other")] 
context5_2 <- context5_1[which(context5_1$Pclass == 3)]
context5_3 <- context5_2[which(context5_2$SibSp == 0)]
context5 <- context5_3[is.na(context5_3$Age)] 

context5$Age <- mean(context5_c$Age)


context6_1 <- test[which(test$Title == "Mr.")] 
context6_2 <- context6_1[which(context6_1$Pclass == 2)]
context6_3 <- context6_2[which(context6_2$SibSp == 0)]
context6 <- context6_3[is.na(context6_3$Age)] 

context6$Age <- mean(context6_c$Age)


context7_1 <- test[which(test$Title == "Mrs.")] 
context7_2 <- context7_1[which(context7_1$Pclass == 2)]
context7_3 <- context7_2[which(context7_2$SibSp == 0)]
context7 <- context7_3[is.na(context7_3$Age)] 

context7$Age <- mean(context7_c$Age)


context8_1 <- test[which(test$Title == "Mr.")] 
context8_2 <- context8_1[which(context8_1$Pclass == 1)]
context8_3 <- context8_2[which(context8_2$SibSp == 0)]
context8 <- context8_3[is.na(context8_3$Age)] 

context8$Age <- mean(context8_c$Age)


context9_1 <- test[which(test$Title == "Mrs.")] 
context9_2 <- context9_1[which(context9_1$Pclass == 1)]
context9_3 <- context9_2[which(context9_2$SibSp == 0)]
context9 <- context9_3[is.na(context9_3$Age)] 

context9$Age <- mean(context9_c$Age)


context10_1 <- test[which(test$Title == "Master.")] 
context10_2 <- context10_1[which(context10_1$Pclass == 3)]
context10_3 <- context10_2[which(context10_2$SibSp == 1)]
context10 <- context10_3[is.na(context10_3$Age)] 

context10$Age <- mean(context10_c$Age)


context11_1 <- test[which(test$Title == "Miss.")] 
context11_2 <- context11_1[which(context11_1$Pclass == 3)]
context11_3 <- context11_2[which(context11_2$SibSp == 1)]
context11 <- context11_3[is.na(context11_3$Age)] 

context11$Age <- mean(context11_c$Age)


context12_1 <- test[which(test$Title == "Mr.")] 
context12_2 <- context12_1[which(context12_1$Pclass == 3)]
context12_3 <- context12_2[which(context12_2$SibSp == 1)]
context12 <- context12_3[is.na(context12_3$Age)] 

context12$Age <- mean(context12_c$Age)


context13_1 <- test[which(test$Title == "Mrs.")] 
context13_2 <- context13_1[which(context13_1$Pclass == 3)]
context13_3 <- context13_2[which(context13_2$SibSp == 1)]
context13 <- context13_3[is.na(context13_3$Age)] 

context13$Age <- mean(context13_c$Age)


context14_1 <- test[which(test$Title == "Miss.")] 
context14_2 <- context14_1[which(context14_1$Pclass == 3)]
context14_3 <- context14_2[which(context14_2$SibSp == 2)]
context14 <- context14_3[is.na(context14_3$Age)] 

context14$Age <- mean(context14_c$Age)


context15_1 <- test[which(test$Title == "Mr.")] 
context15_2 <- context15_1[which(context15_1$Pclass == 3)]
context15_3 <- context15_2[which(context15_2$SibSp == 2)]
context15 <- context15_3[is.na(context15_3$Age)] 

context15$Age <- mean(context15_c$Age)


context16_1 <- test[which(test$Title == "Miss.")] 
context16_2 <- context16_1[which(context16_1$Pclass == 3)]
context16_3 <- context16_2[which(context16_2$SibSp == 8)]
context16 <- context16_3[is.na(context16_3$Age)] 

context16$Age <- mean(context16_c$Age)


test_total <- rbind(test_full, context1, context2, context3, context4, context5, context6, context7, context8, context9, context10, context11, context12, context13, context14, context15, context16)

View(test_total)
summary(test_total)



answer <- c(rep(-1, nrow(test_total)))
answer <- predict(model_final, test_total, "response")

test_total$Prediction <- answer
test_total$Survived <- ifelse(test_total$Prediction > 0.5, 1, 0)


View(test_total)

Submission <- cbind(test_total[, 1], test_total[, 12])
  
View(Submission)

summary(Submission)



###########################################################################################
# write data to a file

write.csv(Submission, file = "Submission_V1.csv", row.names = FALSE)



