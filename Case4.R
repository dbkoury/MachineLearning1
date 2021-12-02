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
#(Intercept)         age GenderWoman 
#0.1169510   1.0144292   0.7407149


#provide insight into what the results mean
# For an increase 1 year in the patient's age, the odds of the patients getting intubated 
#increases by a factor of 1.0144. When a patient's gender is female, the odds of the patients getting 
#intubated increases by a factor of 0.7407.

###PART 2:
divideData <- createDataPartition(data$intubated,p=.1,list=F)
train <- data[divideData,]
test <- data[-divideData,]

##Logistic Model
logisticmodel <- glm(intubated~.,data=train, family=binomial)

probs <- predict(logisticmodel, type="response", newdata=test)
#NOTE: Although our model ran, a warning message was presented informing us that the fit was rank-deficient
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
#We correctly identified 46417 patients as not requiring intubation.
#True Negatives: When we correctly identify an observation for needing intubation
(logD <- logtable[4]) #3
#We correctly identified 3 patients who needed intubation.
#False Positives: When we wrongfully identify an observation as not needing intubation (but they do)
(logB <- logtable[3]) #10320
#We incorrectly assumed that 10320 patients did not need intubation, when they actually did need intubation.
#False Negatives: When we wrongfully identify an observation for needing intubation (but they don't)
(logC <- logtable[2]) #4
#We incorrectly assumed that 4 patients needed intubation, when they did not. 


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
  #Our balanced accuracy is 0.5001022. As it can be seen in the 


#LDA Model
preprocessing <- train %>% preProcess(method=c("center","scale"))
traintransformed <- preprocessing %>% predict(train)
testtransformed <- preprocessing %>% predict(test)

ldamodel <- lda(intubated~.-Gender-labResults,data=traintransformed)

predictions <- ldamodel %>% predict(testtransformed)
(ldatable <- table(predictions$class,testtransformed$intubated, dnn=c("Predicted","Observed")))

(ldaaccuracy <- mean(predictions$class==testtransformed$intubated))
##Accuracy Rate = 0.8180777
#This model does  somewhat good job at predicting if someone is not intubated. 
#It is a bit worse than Logistic model as its metric was about 99%.
(ldaerror <- 1-ldaaccuracy)
##Error Rate = 0.1819223
  #However, its error rate is lower than the logistic model.
  #Nevertheless, itstill a slightly higher error rate than simply assigning no to every observation.

#True Positives
(ldaA <- ldatable[1]) #46420
#We correctly identified 46420 patients as not requiring intubation.
#True Negatives
(ldaD <- ldatable[4]) #1
#We correctly identified 1 patient who needed intubation.
#False Positives
(ldaB <- ldatable[3]) #10322
#We incorrectly assumed that 10322 patients did not need intubation, when they actually did need intubation.
#False Negatives
(ldaC <- ldatable[2]) #1
#We incorrectly assumed that 1 patient needed intubation, when they did not. 


#Sensitivity
(ldaSensitivity <- ldaA/(ldaA+ldaC)) #0.9999785
#This model, like the logistic model does a great job predicting if someone is not intubated.
#The metric acccounted for nearly all of the accuracy in the model (99.99%)
#Specificity
(ldaSpecificity <- ldaD/(ldaB+ldaD)) #0.00009687106
#But it doesn't do that good of a job at predicting if someone is intubated (similar to logistics model).
#This makes sense as LDA and logistics model are similar to one another. Hence having similar
#sensitivity and specificity happens by the modeles' nature. 
#Balanced Accuracy
(ldaBalanced <- (ldaSensitivity+ldaSpecificity)/2)
#Our balanced accuracy is 0.5000377

#QDA Model
#qdamodel <- qda(intubated~.,data=traintransformed)
  #This model would not run for us, as the error message informs us of  a rank deficiency in the "No" group
#This was foreshadowed by our previous error message in the logistic model

#Rank deficiency is "when one or more of the independent variables are a linear function of the other independent variables in the model."
#i.e., multicollinearity, which is one of our assumptions. In order to test for this we conducted a vif-test on our logistic model
#car::vif(logisticmodel)
#We produced an error with this, meaning it may be likely we have perfect multicollineaity
summary(c(data$Gender, data$pregnant))
#A closer look at the data and it seems that the "Gender" and "Pregnant" variables may be the issue,
#as the "gender" variable is redundant as every male observation is "Not Applicable" under the "Pregnant" variable, meaning they have the same value
#Let's rerun the model without Gender and then test again for multicollinearity
logisticmodel2 <- glm(intubated~.-Gender,data=train, family=binomial)
#car::vif(logisticmodel2)
#The model still would not run meaning there is still collinearity somewhere.
#We were able to discover what other variables were multicollinear with a rather rudimentary method
#We just plugged in variables into the QDA model to see which ones broke it.
#Interestingly we found the two other variables that could not coexist were 
#"LabResults" and "finalClassification"
#Looking back the reason was rather intuitive, similarly to the prior issue,
#final classification is just a more broken down version of the lab results.
#Looking at the summary
summary(c(data$labResults, data$finalClassification))
#You can see that the "No" category is the same as the "Negative Case" category.
#We ultimately decided should we have dropped a variable, to run the model
#sticking with the "finalClassification" would be the best course of action
#as it is more fine data with more categories
#as well as an updated ideally more accurate version of the original "labResults"
#Including both would also violate our assumption of Independence of Errors,
#As having a second covid test is a repeated measurement.
logisticmodel <- glm(intubated~.-Gender-labResults,data=train, family=binomial)
car::vif(logisticmodel)
#And now as you can see our vif test runs and all the variables look good
qdamodel <- qda(intubated~.-Gender-labResults,data=traintransformed)
#In fact when we run the qdamodel without these variables the model runs without issue.

#KNN Model
set.seed(666)
knnmodel <- train(intubated~., data=traintransformed, method="knn")
knnpred <- predict(knnmodel, newdata=testtransformed)
confusionMatrix(knnpred, testtransformed$intubated)
knntable <- table(knnpred,testtransformed$intubated)

(knnaccuracy <- mean(knnpred==testtransformed$intubated))
##Accuracy Rate = 0.8095305
#This model gives us the lowest accuracy rate. 
(knnerror <- 1-knnaccuracy)
##Error Rate = 0.1904695
#Since error rate is directly related to accuracy rate, we see that the model's low accuracy rate reflects on the error rate too. 

#True Positives
(knnA <- knntable[1]) #45583
#True Negatives
(knnD <- knntable[4]) #353
#False Positives
(knnB <- knntable[3]) #9970
#False Negatives
(knnC <- knntable[2]) #838

#Sensitivity
(knnSensitivity <- knnA/(knnA+knnC)) #0.9819478
#This model does a great job predicting if someone is not intubated. Although it's not as high as other models (98% vs. 99%), it does account for most of the accuracy. 
#Specificity
(knnSpecificity <- knnD/(knnB+knnD)) #0.03419549
#Although 0.03 is a low number on paper, it is the highest specificity rate that we have gotten so far. Therefore, this model does the best job at prediciting if someone is intubated.
#Which, in the beginning of the case, we mentioned is a more critical rate since its consequences are more dire.  
#Balanced Accuracy
(knnBalanced <- (knnSensitivity+knnSpecificity)/2) #0.5080717

###CONCLUSION:

#best model and shape of data 
#As previously mentioned, we looked at the Specificity rate as it is supposed to predict whether someone needs to and is intubated. The highest 
#specificity rate was of KNN model, hence we believe the KNN model is the best model. KNN is a completely non-parametric approach since no 
#assumptions are made about the shape of decision boundary. Since we had the highest specificity rate in KNN, we can assume
#that the shape of data is very non-parametric. Hail Hydra🐙
##In Dylan's opinion we should use the QDA results because all of these models are crap and we would be better off pulling something out of our ass or just paying the money to give everyone an intubator. Hail Satan. Free Love.
