###CASE 4
##Team 2: Anoushka Mahar, Riti Dabas, Dylan Koury

###SET-UP:
rm(list=ls())
setwd("C:/Users/smrit/Downloads/School/MSBA 4.0/R files")
library(caret)
library(tidyverse)
library(mltools)
library(MASS)
data <- read.csv("Case4.csv", stringsAsFactors = TRUE)
set.seed(666)

###PART 1:

shortlogmodel <- glm(intubated~ age + Gender, data=data, family=binomial)
summary(shortlogmodel)
#Both of our variables are significant at an alpha of .01
options(scipen=999)
#However for our interpretation we will convert our coefficients to odds.
exp(shortlogmodel$coefficients)
#interpret the results of coefficients
#(Intercept)         age GenderWoman 
#0.1169510   1.0144292   0.7407149

#The intercept implies that for our base (man at the age of 0) the odds of being intubated are 0.1169510 to 1
# For an increase 1 year in the patient's age, the odds of the patients getting intubated increases by a factor of 1.0144.
1/0.7407149
#Our Gender Variable uses being a woman as the base, so the log odds implies that
#the odds of being intubated as a woman are 0.7407 times less likely than as a man
#Stated differently, being a man increases the odds of intubation by a factor of 1.350047 

#These results imply that older people and men are more likely to be intubated
#This aligns with the knowledge we have gained during the pandemic, 
#where older people are significantly more vulnerable to developing COVID-19 
#due to weaker immunity system and physiological changes that come with ageing and
#potential underlying health conditions.
#Given what we know about the most at risk populations, there tends to be a heavy skew towards covid deaths of older patients
#It would make sense that hospitals are targeting older citizens for intubator use

#Like many cultures, traditional family in Mexico is deeply rooted in respecting their elders.
#This also includes that it is the duty and responsibility of the younger family members to 
#take care of elder members of the family.
#This cultural construct may also influence patient selection for intubators.

#A number of medical studies have also begun to find evidence supporting the notion that the 
#male population is more at risk of covid-19
#Therefore once again it would make sense that hospitals are targeting this slice of the population for intubators

###PART 2:
divideData <- createDataPartition(data$intubated,p=.1,list=F)
train <- data[divideData,]
test <- data[-divideData,]

##Logistic Model
logisticmodel <- glm(intubated~.,data=train, family=binomial)

probs <- predict(logisticmodel, type="response", newdata=test)
#NOTE: Although our model ran, a warning message was presented informing us that the fit was rank-deficient.
pred <- ifelse(probs>0.5, "Yes", "No")
(logtable <- table(pred,test$intubated,dnn=c("Predicted","Observed")))
(logaccuracy <- mean(pred==test$intubated))
##Accuracy Rate = 0.8180601
(logerror <- 1-logaccuracy)
##Error Rate = 0.1819399
  #With our Logistic Model, we predicted 81.806% of the observations accurately and 18.19% inaccurately

mean(data$intubated=="No") #0.818078
  #Interestingly, we would have a slightly higher accuracy rate by predicting every observation as "no" than this model gives us
  #Therefore, it is not an effective model to rely on.

#True Positives: When we correctly identify an observation for not needing intubation
(logA <- logtable[1]) #46417
#The True Positive contains 81.8% of the data, which it is correctly identifying as not needing intubation
#Which contributed to the bulk of the accuracy rate of the model.
#Model correctly predicts 46417 patients as not requiring intubation which is a good amount
#True Negatives: When we correctly identify an observation for needing intubation
(logD <- logtable[4]) #3
#But very very few negatives as we only correctly identified 3 patients who needed intubation.
#False Positives: When we wrongfully identify an observation as not needing intubation (but they do)
(logB <- logtable[3]) #10320
#However, the trade off is that it is more often assuming positive as it incorrectly mislabels 10320 patients as not needing intubation, when they actually did need intubation
#False Negatives: When we wrongfully identify an observation for needing intubation (but they don't)
(logC <- logtable[2]) #4
#Model only incorrectly labels a few positives as negatives, but has more false negatives(4) than true negatives(3)


#Sensitivity
(logSensitivity <- logA/(logA+logC)) #0.9999138
#This model does a great job predicting if someone is not intubated (our positive class)
#This metric accounts for nearly all the accuracy in the model. (99.99%)
#Specificity
(logSpecificity <- logD/(logB+logD)) #0.0002906132
#But it does a terrible job prediciting if someone is intubated (our negative class)
#We would argue that the accuracy of this metric is more critical, as if someone who is not intubated but should have been could lead to death,
#Whereas if someone who is intubated but should not have been is just a waste of resources. 
#Balanced Accuracy
(logBalanced <- (logSensitivity+logSpecificity)/2)
#Our balanced accuracy is 0.5001022 or 50% (as sensitivity is almost 100% and specificity is nearly 0%)

#LDA Model
preprocessing <- train %>% preProcess(method=c("center","scale"))
traintransformed <- preprocessing %>% predict(train)
testtransformed <- preprocessing %>% predict(test)

ldamodel <- lda(intubated~.-Gender-labResults,data=traintransformed)

predictions <- ldamodel %>% predict(testtransformed)
(ldatable <- table(predictions$class,testtransformed$intubated, dnn=c("Predicted","Observed")))

(ldaaccuracy <- mean(predictions$class==testtransformed$intubated))
##Accuracy Rate = 0.8180777
#Slightly higher accuracy rate than the logistic model
(ldaerror <- 1-ldaaccuracy)
##Error Rate = 0.1819223
#Slightly Lower error rate than the logistic model, still a slightly higher error rate than simply assigning no to every observation (0.181922)

#True Positives
(ldaA <- ldatable[1]) #46420
#We correctly identified 46420 patients as not requiring intubation.
#Meaning this model accurately predicted three more positives than the logistic model
#True Negatives
(ldaD <- ldatable[4]) #1
#We only correctly identified 1 patient who needed intubation.
#This model accurately predicts two less negatives than the logistic model
#False Positives
(ldaB <- ldatable[3]) #10322
#We incorrectly assumed that 10322 patients did not need intubation, when they actually did need intubation.
#Actually 2 more false positives than before
#False Negatives
(ldaC <- ldatable[2]) #1
#We incorrectly assumed that 1 patient needed intubation, when they did not.
#This meant three fewer false negatives (except the model still only predicts two total negatives)

#Sensitivity
(ldaSensitivity <- ldaA/(ldaA+ldaC)) #0.9999785
#This model, like the logistic model does a great job predicting if someone is not intubated.
#The metric acccounted for nearly all of the accuracy in the model (99.99%)
#This model actually has a higher sensitivity than the logistic model, as seen it does a better job at predicting positives
#Specificity
(ldaSpecificity <- ldaD/(ldaB+ldaD)) #0.00009687106
#But this model does not accurately predict if someone is intubated (similar but worse than the logistics model).
#This makes sense as LDA and logistics model are similar to one another. Hence having similar
#sensitivity and specificity happens by the models' nature. 

#Balanced Accuracy
(ldaBalanced <- (ldaSensitivity+ldaSpecificity)/2)
#Our balanced accuracy is 0.5000377
#Again this is about 50%, which makes sense given the near 100%/0% trade-off

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
#This model gives us the lowest accuracy rate
(knnerror <- 1-knnaccuracy)
##Error Rate = 0.1904695
#Since error rate is directly related to accuracy rate, we see that the model's low accuracy rate reflects on the higher error rate too.


#True Positives
(knnA <- knntable[1]) #45583
#This model predicts the fewest number of positives correctly
#True Negatives
(knnD <- knntable[4]) #353
#However it correctly predicts about 350 more negatives than the other models
#False Positives
(knnB <- knntable[3]) #9970
#There is also a lower number of false positives
#False Negatives
(knnC <- knntable[2]) #838
#With the trade off of many more false negatives
#This pattern makes sense because this model appears to select more variables as negative than the others,
#meaning there is likely to be more selection error

#Sensitivity
(knnSensitivity <- knnA/(knnA+knnC)) #0.9819478
#This model still does a great job predicting if someone is not intubated. Although it's not as high as other models
#In comparison, we've lost about 1-2% of our sensitivity
#Specificity
(knnSpecificity <- knnD/(knnB+knnD)) #0.03419549
#However, we have traded that sensitivity for a 3-4% higher specificity, the highest we've gotten so far
#Although 0.03 is a low number on paper, given the other models predicted less than 5 negatives at all, this is a solid improvement
#Therefore, this model does the best job at predicting if someone is intubated.
#Which, in the beginning of the case, we mentioned is a more critical rate since its consequences are more dire.
#Balanced Accuracy
(knnBalanced <- (knnSensitivity+knnSpecificity)/2) #0.5080717
#Ultimately this trade off has given us a higher balanced accuracy
#While it has only gone up less than a percentage, 
#this increase is reflective of the KNN model increasing the specificity measurement

###CONCLUSION:

#Based solely upon accuracy, the "best" model would realistically be none of them,
#In fact, as mentioned earlier, we would achieve a higher accuracy rate than every model by simply assuming that every observation was negative (not intubated)
#However in truth we believe a holistic approach would be better, one that considers the accuracy of the model's ability to guess both positive and negative observations correctly
#This is especially critical in context of the data.
#Our models have a high accuracy when predicting non intubated observations, they have a much lower accuracy when predicting intubated observations
#Our data deals with covid patients, and while incorrectly predicted nonintubated patients may lead to a misuse/waste of resources, incorrectly predicting someone who needs an intubator as not could lead to death
#Because of this, we chose the best model based upon the balanced accuracy metric that considers both equally.
#Simply assigning every observation as non intubated would produce a balanced accuracy of 50%
#Every single model outperformed 50%,
#However the model with the highest balanced accuracy was our K-Nearest Neighbors model, due to it having the highest specificity rate of 3.419% (accurately predicting which patients need to be intubated)
#hence we believe the KNN model is the best model
#Given the difficultly of our logistic and lda models to accurately predict any negative observations,
#It is likely that the shape of the decision boundary is not linear,
#And K-Nearest Neighbors makes no assumptions about the shape of the decision boundary and is therefore a non-parametric approach
#This is likely why it was able to accurately predict 350 more negative observations than the other models, giving it the highest specificity rate.
#With an increase in correct prediction of those that require medical attention, our chosen model would ultimately increase care and assistance to those in need better than the other models.
#While our number is still rather low considering the massive amount of observations used in the testing data, each one of those values is a potential life at risk
#that may have not received medical attention if we used a different model with a lower Specificity rate. 
