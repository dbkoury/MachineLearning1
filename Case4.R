###CASE 4
##Team 2: Anoushka Mahar, Riti Dabas, Dylan Koury

###SET-UP:
rm(list=ls())
setwd("C:/Users/dylan/MSBA/ML1/Assignments")
library(caret)
library(tidyverse)
library(mltools)
library(MASS)
data <- read.csv("Case4.csv", stringsAsFactors = TRUE)
set.seed(666)

###PART 1:

shortlogmodel <- glm(intubated~ age + Gender, data=data, family=binomial)
summary(shortlogmodel)
options(scipen=999)
exp(shortlogmodel$coefficients)

###PART 2:
divideData <- createDataPartition(data$intubated,p=.1,list=F)
train <- data[divideData,]
test <- data[-divideData,]

##Logistic Model
logisticmodel <- glm(intubated~.,data=train, family=binomial)
probs <- predict(logisticmodel, type="response", newdata=test)
pred <- ifelse(probs>0.5, "Yes", "No")
(logtable <- table(pred,test$intubated,dnn=c("Predicted","Observed")))
(logaccuracy <- mean(pred==test$intubated))
##Accuracy Rate = 0.8180601
(logerror <- 1-logaccuracy)
##Error Rate = 0.1819399

mean(data$intubated=="No") #0.818078
#In fact, we would actually have a slightly higher accuracy rate just predicting every observation as no than this model gives us

#True Positives: When we correctly identify an observation for not needing intubation
(logA <- logtable[1]) #46417
#True Negatives: When we correctly identify an observation for needing intubation
(logD <- logtable[4]) #3
#False Positives: When we wrongfully identify an observation as not needing intubation (but they do)
(logB <- logtable[3]) #10320
#False Negatives: When we wrongfully identify an observation for needing intubation (but they don't)
(logC <- logtable[2]) #4

#Sensitivity
(logSensitivity <- logA/(logA+logC)) #0.9999138
  #This model does a great job predicting if someone is not intubated (our positive class)
  #This metric accounts for nearly all the accuracy in the model. (99.99%)
#Specificity
(logSpecificity <- logD/(logB+logD)) #0.0002906132
  #But it does a terrible job prediciting if someone is intubated (our negative class)
  #We would argue that the accuracy of this metric is more critical, as if someone who is not intubated but should have been could lead to death,
  #Whereas if someone who is intubated but should not have been is just a waste of resources

(logBalanced <- (logSensitivity+logSpecificity)/2)
  #Our balanced accuracy is 0.5001022


#LDA Model
preprocessing <- train %>% preProcess(method=c("center","scale"))
traintransformed <- preprocessing %>% predict(train)
testtransformed <- preprocessing %>% predict(test)

ldamodel <- lda(intubated~.,data=traintransformed)

predictions <- ldamodel %>% predict(testtransformed)
(ldatable <- table(predictions$class,testtransformed$intubated, dnn=c("Predicted","Observed")))

(ldaaccuracy <- mean(predictions$class==testtransformed$intubated))
##Accuracy Rate = 0.8180777
(ldaerror <- 1-ldaaccuracy)
##Error Rate = 0.1819223
  #Slightly Lower than the logistic model, still a slightly higher error rate than simply assigning no to every observation

#True Positives
(ldaA <- ldatable[1]) #46420
#True Negatives
(ldaD <- ldatable[4]) #1
#False Positives
(ldaB <- ldatable[3]) #10322
#False Negatives
(ldaC <- ldatable[2]) #1

(ldaSensitivity <- ldaA/(ldaA+ldaC)) #0.9999785

(ldaSpecificity <- ldaD/(ldaB+ldaD)) #0.00009687106

(ldaBalanced <- (ldaSensitivity+ldaSpecificity)/2)
#Our balanced accuracy is 0.5000377

#QDA Model
#qdamodel <- qda(intubated~.,data=traintransformed)
  #This model would not run for us
  #INSERT RATIONALE

#KNN Model
set.seed(666)
knnmodel <- train(intubated~., data=traintransformed, method="knn")
knnpred <- predict(knnmodel, newdata=testtransformed)
confusionMatrix(knnpred, testtransformed$intubated)
knntable <- table(knnpred,testtransformed$intubated)

(knnaccuracy <- mean(knnpred==testtransformed$intubated))
##Accuracy Rate = 0.8095305
(knnerror <- 1-knnaccuracy)
##Error Rate = 0.1904695
  #This model gives us the lowest accuracy rate

#True Positives
(knnA <- knntable[1]) #45583
#True Negatives
(knnD <- knntable[4]) #353
#False Positives
(knnB <- knntable[3]) #9970
#False Negatives
(knnC <- knntable[2]) #838

(knnSensitivity <- knnA/(knnA+knnC)) #0.9819478

(knnSpecificity <- knnD/(knnB+knnD)) #0.03419549

(knnBalanced <- (knnSensitivity+knnSpecificity)/2) #0.5080717

###CONCLUSION:

##In Dylan's opinion we should use the QDA results because all of these models are crap and we would be better off pulling something out of our ass or just paying the money to give everyone an intubator. Hail Satan. Free Love.
