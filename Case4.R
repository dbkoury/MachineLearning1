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

#interpret the results of coefficients
#provide insight into what the results mean

###PART 2:
divideData <- createDataPartition(data$intubated,p=.1,list=F)
train <- data[divideData,]
test <- data[-divideData,]

##Logistic Model
logisticmodel <- glm(intubated~.,data=train, family=binomial)

##Assumptions
#Linearity of the Logit
  #The only continuous variable is age
plot(data$age,log(data$age))
interaction <- data$age*log(data$age)
linearitylog <- glm(intubated~interaction, family=binomial,data=data)
summary(linearitylog)
#Looking at the graph of the age variable against its log values,
#As well as the significance of the interaction term between them when modeled with intubation,
#We can see that the inclusion of the age variable would violate our assumptions

#Multicollinearity
#car::vif(logisticmodel)
#We produced an error with this, meaning it may be likely we have perfect multicollineaity
summary(c(data$Gender, data$pregnant))
#A closer look at the data and it seems that the "Gender" and "Pregnant" variables may be the issue,
#as the "gender" variable is redundant as every male observation is "Not Applicable" under the "Pregnant" variable
#Let's rerun the model without Gender and then test again for multicollinearity
logisticmodel <- glm(intubated~.-Gender,data=train, family=binomial)
#car::vif(logisticmodel)
#The model still would not run meaning there is still collinearity somewhere.
#Looking ahead, you will see later that we had trouble getting our QDA function to run,
#this was due to rank deficiency, or rather multicollinearity. 
#We were able to discover what other variables were multicollinear with a rather rudimentary method
#We just plugged in variables into the model to see which ones broke it.
#Interestingly we found the two other variables that could not coexist were 
#"LabResults" and "finalClassification"
#Looking back the reason was rather intuitive, similarly to the prior issue,
#final classification is just a more broken down version of the lab results.
#Looking at the summary
summary(c(data$labResults, data$finalClassification))
#You can see that the "No" category is the same as the "Negative Case" category.
#We ultimately chose to stick with the "finalClassification" as it is more fine data with more categories
#as well as an updated ideally more accurate version of the original "labResults"
#Including both would also violate our assumption of Independence of Errors,
#As having a second covid test is a repeated measurement.
logisticmodel <- glm(intubated~.-Gender-labResults,data=train, family=binomial)
car::vif(logisticmodel)
#And now as you can see our vif test runs and all the variables look good

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
#Balanced Accuracy
(logBalanced <- (logSensitivity+logSpecificity)/2)
  #Our balanced accuracy is 0.5001022


#LDA Model
preprocessing <- train %>% preProcess(method=c("center","scale"))
traintransformed <- preprocessing %>% predict(train)
testtransformed <- preprocessing %>% predict(test)

ldamodel <- lda(intubated~.-Gender-labResults,data=traintransformed)

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
#Sensitivity
(ldaSensitivity <- ldaA/(ldaA+ldaC)) #0.9999785
#Specificity
(ldaSpecificity <- ldaD/(ldaB+ldaD)) #0.00009687106

#Balanced Accuracy
(ldaBalanced <- (ldaSensitivity+ldaSpecificity)/2)
#Our balanced accuracy is 0.5000377

#QDA Model
#qdamodel <- qda(intubated~.,data=traintransformed)
  #This model would not run for us, as there appears to be a rank deficiency in the "No" group
#As discussed earlier, we had to edit the model to get it to run properly
qdamodel <- qda(intubated~.-Gender-labResults,data=traintransformed)

qdapredictions <- qdamodel %>% predict(testtransformed)
#Accuracy Rate - 
(qdaaccuracy <- mean(qdapredictions$class == testtransformed$intubated))
#Error Rate - 
(qdaerror <- 1-qdaaccuracy)
# QDA model actually did worse with a 0.8577778 accuracy rate given proportion of .8 and set seed of 1000 - likely overfitting the data
(qdatable <- table(predictions$class,testtransformed$intubated, dnn=c("Predicted","Observed")))

#True Positives
(qdaA <- qdatable[1]) #
#True Negatives
(qdaD <- qdatable[4]) #
#False Positives
(qdaB <- qdatable[3]) #
#False Negatives
(qdaC <- qdatable[2]) #
#Sensitivity
(qdaSensitivity <- qdaA/(qdaA+qdaC)) #
#Specificity
(qdaSpecificity <- qdaD/(qdaB+qdaD)) #

#Balanced Accuracy
(qdaBalanced <- (qdaSensitivity+qdaSpecificity)/2)
#Our balanced accuracy is 

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

#Sensitivity
#Specificity
(knnSensitivity <- knnA/(knnA+knnC)) #0.9819478

(knnSpecificity <- knnD/(knnB+knnD)) #0.03419549

(knnBalanced <- (knnSensitivity+knnSpecificity)/2) #0.5080717

###CONCLUSION:

#best model
#shape of data (linear to non-parametric)
##In Dylan's opinion we should use the QDA results because all of these models are crap and we would be better off pulling something out of our ass or just paying the money to give everyone an intubator. Hail Satan. Free Love.
