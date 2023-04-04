#preparations -------------------------------------------------------------------------
library(tidyverse)
library(fastDummies)
library(visdat)
library(caret) #confusion matrix package
library(rpart.plot)
library(rpart)

trainingData <- read.csv("/Users/nikhilbiyyap/Downloads/trainingData.csv")
str(trainingData)
library(ggplot2)
library(GGally)

trainingData <- subset(trainingData, select = -id)
trainingData <- subset(trainingData, select =  -toCoupon_GEQ5min)

trainingData$age <- ifelse(trainingData$age == "50plus", 51, trainingData$age)
trainingData$age <- ifelse(trainingData$age == "below21", 20, trainingData$age)

# Assign different values with weightage to the "car" column
# Determine non-blank values
non_blank <- trainingData$car[trainingData$car != ""]
# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)
# Generate random indices for blank values
blank_indices <- which(trainingData$car == "")
# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$car[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)



# Assign different values with weightage to the "Bar" column
# Determine non-blank values
non_blank <- trainingData$Bar[trainingData$Bar != ""]
# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)
# Generate random indices for blank values
blank_indices <- which(trainingData$Bar == "")
# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$Bar[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)


# Assign different values with weightage to the "CoffeeHouse" column
# Determine non-blank values
non_blank <- trainingData$CoffeeHouse[trainingData$CoffeeHouse != ""]
# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)
# Generate random indices for blank values
blank_indices <- which(trainingData$CoffeeHouse == "")
# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$CoffeeHouse[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)


# Assign different values with weightage to the "CarryAway" column
# Determine non-blank values
non_blank <- trainingData$CarryAway[trainingData$CarryAway != ""]
# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)
# Generate random indices for blank values
blank_indices <- which(trainingData$CarryAway == "")
# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$CarryAway[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)


# Assign different values with weightage to the "RestaurantLessThan20" column
# Determine non-blank values
non_blank <- trainingData$RestaurantLessThan20[trainingData$RestaurantLessThan20 != ""]
# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)
# Generate random indices for blank values
blank_indices <- which(trainingData$RestaurantLessThan20 == "")
# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$RestaurantLessThan20[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)



# Assign different values with weightage to the "Restaurant20To50" column
# Determine non-blank values
non_blank <- trainingData$Restaurant20To50[trainingData$Restaurant20To50 != ""]
# Calculate probability of each non-blank value
prob <- table(non_blank) / length(non_blank)
# Generate random indices for blank values
blank_indices <- which(trainingData$Restaurant20To50 == "")
# Assign each blank value with a non-blank value randomly selected based on their probabilities
trainingData$Restaurant20To50[blank_indices] <- sample(names(prob), length(blank_indices), replace = TRUE, prob = prob)

summary(trainingData)

trainingData$destination <- as.factor(trainingData$destination)
trainingData$passanger <- as.factor(trainingData$passanger)
trainingData$weather <- as.factor(trainingData$weather)
trainingData$temperature <- as.numeric(trainingData$temperature)
trainingData$time <- as.factor(trainingData$time)
trainingData$coupon <- as.factor(trainingData$coupon)
trainingData$expiration <- as.factor(trainingData$expiration)
trainingData$gender <- as.factor(trainingData$gender)
trainingData$maritalStatus <- as.factor(trainingData$maritalStatus)
trainingData$has_children <- as.factor(trainingData$has_children)
trainingData$education <- as.factor(trainingData$education)
trainingData$Y <- as.factor(trainingData$Y)
trainingData$direction_opp <- as.factor(trainingData$direction_opp)
trainingData$direction_same <- as.factor(trainingData$direction_same)
trainingData$toCoupon_GEQ25min <- as.factor(trainingData$toCoupon_GEQ25min)
trainingData$toCoupon_GEQ15min <- as.factor(trainingData$toCoupon_GEQ15min)
trainingData$age <- as.numeric(trainingData$age)
trainingData$occupation <- as.factor(trainingData$occupation)
trainingData$income <- as.factor(trainingData$income)
trainingData$car <- as.factor(trainingData$car)
trainingData$Bar <- as.factor(trainingData$Bar)
trainingData$CoffeeHouse <- as.factor(trainingData$CoffeeHouse)
trainingData$CarryAway <- as.factor(trainingData$CarryAway)
trainingData$RestaurantLessThan20 <- as.factor(trainingData$RestaurantLessThan20)
trainingData$Restaurant20To50 <- as.factor(trainingData$Restaurant20To50)


# MODELING.  Let introduce some advance feature and best practices for replicability
outcomeName <- 'Y'
predictorNames <- names(trainingData)[names(trainingData) != outcomeName & !names(trainingData) %in% c("CarryAway", "CoffeeHouse", "RestaurantLessThan20", "Restaurant20To50")]
# creating a list of features 
#to be included in the model
#Useful with long number of features

set.seed(1234)  # setting seed to reproduce results of random sampling
split<-(.80)
library (caret)
index <- createDataPartition(trainingData$Y, p=split, list=FALSE) # row indices for training data

train.df <- trainingData[ index,]  # model training data
test.df<- trainingData[ -index,]   # test data

# model setup
names(getModelInfo())  #200+ ML algorithms

modelLookup(model='rf')  # To find the parameters of a model that can be tuned
modelLookup(model='gbm') 



fitControl <- trainControl(method = "none")   # control parameters for training
# see help(trainControl) for details


### RF Model
rf<-train(train.df[,predictorNames],train.df[,outcomeName],
          method='rf',
          trControl=fitControl)

gbm<-train(train.df[,predictorNames],train.df[,outcomeName],
           method='gbm',
           trControl=fitControl)

# summarizing the models
rfImp<-varImp(rf)  # computes variable importance for regression and classification models
rfImp
plot(rfImp)

gbmImp<-varImp(gbm) #generates error.  run with line below
gbmImp<-summary(gbm)
gbmImp
plot(gbmImp)


# measuring performance
rf.predict<-predict(rf,test.df[,predictorNames],type="raw")
levels(test.df[, outcomeName])
confusionMatrix(rf.predict,test.df[,outcomeName], positive = "1")

gbm.predict<-predict(gbm,test.df[,predictorNames],type="raw")
confusionMatrix(gbm.predict,test.df[,outcomeName], positive = "1")

# draw ROC curve and perform visual check for better accurancy/performacen
library(pROC)
gbm.probs <- predict(gbm,test.df[,predictorNames],type="prob")    
rf.probs <- predict(rf,test.df[,predictorNames],type="prob") 

gbm.plot<-plot(roc(test.df$Y,gbm.probs[,2]))
rf.plot<-lines(roc(test.df$Y,rf.probs[,2]), col="blue")
legend("bottomright", legend=c("rf", "gbm"), col=c("blue", "black"), lwd=2)  # we can see who RF outperfoms GBM

#Model tuning
modelLookup(model='gbm') 
help("trainControl")

fitControl.gbm <- trainControl(method = "cv",
                               number = 20,
                               sampling = "up")   # control parameters for training
# see help(trainControl) for details

gbm.tuned<-train(train.df[,predictorNames],train.df[,outcomeName],   #model retraining
                 method='gbm',
                 trControl=fitControl.gbm)

# measuring performance
gbm.tuned.predict<-predict(gbm.tuned,test.df[,predictorNames],type="raw")
confusionMatrix(gbm.tuned.predict,test.df[,outcomeName], positive = "1")
gbm.tuned.probs <- predict(gbm.tuned,test.df[,predictorNames],type="prob")
gbm.tuned.plot<-lines(roc(test.df$Y,gbm.tuned.probs[,2]), col="red")
legend("bottomright", legend=c("rf", "gbm", "gbm.tuned"), col=c("blue", "black", "red"), lwd=2)

#advanced tuning
fitControl.gbm2 <- trainControl(method = "repeatedcv",
                                number = 20,
                                repeats = 5,
                                sampling = "up")

gbm2.tuned<-train(train.df[,predictorNames],train.df[,outcomeName],
                  method='gbm',
                  trControl=fitControl.gbm2)

# measuring performance
gbm2.tuned.predict<-predict(gbm2.tuned,test.df[,predictorNames],type="raw")
confusionMatrix(gbm2.tuned.predict,test.df[,outcomeName], positive = "1")
gbm2.tuned.probs <- predict(gbm2.tuned,test.df[,predictorNames],type="prob")    
gbm2.tuned.plot<-lines(roc(test.df$Y,gbm2.tuned.probs[,2]), col="green")
legend("bottomright", legend=c("rf", "gbm", "gbm.tuned", "gbm2.tuned"), col=c("blue", "black", "red", "green"), lwd=2)

auc(test.df$Y,gbm.probs[,2])
auc(test.df$Y,rf.probs[,2])
auc(test.df$Y,gbm.tuned.probs[,2])
auc(test.df$Y,gbm2.tuned.probs[,2])
