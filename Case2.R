###TEAM NAME: Team 2
##TeamMates: Riti Dabas, Dylan Koury, Anoushka Mahar

#importing packages and prep
options(scipen=999)

#Getting the Data
#setwd("C:/Users/dylan/MSBA/ML1/Assignments")
Case2 <- read.csv("Case2.csv")
attach(Case2)
#Looking at the Data
str(Case2) 
#looking at the structure of our data we can see most of the variables are integers, 
#however some appear to be missing values
summary(Case2) 
#in fact, the summary tells us that most of the columns have around if not significantly over 100 missing values
pairs(Case2[c(-1,-2)]) 
#looking at the pairs, specifically variables paired with FTRetentionRate (as that is the variable we want to look at) 
#none of the plots seem overly linear, which may cause heteroskedasticity issues later

#Deciding Which Variables to Use
cor(Case2[,c(-1,-2)], use="pairwise") 
#Looking at FTRetentionRate
#the highest correlations will tell us which variables may be the most worth investigating 
#with regard to their influence on FTRetentionRate
#The variables with the highest correlations to FTRetentionRate are:
  #AverageSalary: 0.510085373
  #Tenured: 0.474182295
  #TotalFaculty: 0.372126301 

###REGRESSIONS

###VARIABLE 1: Average Salary
#Prediction of what we will get based on the relationship (pos/neg;strong,weak,moderate) when put in context
#actually create model
#discuss model (R^2, p-value, slope)
#describe visually with plots of model and regression
#Test Assumptions
  #Linearity - plot x and y
  #Normalized Residuals - Normal or Skewed
  #Homoskedasticity - BPTest
  #Remaining assumptions with plot of lm

###VARIABLE 2: Tenured
#Prediction of what we will get based on the relationship (pos/neg;strong,weak,moderate) when put in context
  #Given the correlation of Tenured was about .5 (positive, moderate) with FTRetentionRate, 
  #we should expect to see the model to have a positive relationship between these variables as well
#actually create model
lmTenure <- lm(FTRetentionRate ~ Tenured, data=Case2)
#discuss model (R^2, p-value, slope)
summary(lmTenure)
#Equation: FTRetentionRate = 73.5027636 + 0.0178277 * Tenured
  #So there is a positive relationship between these variables, with Retention increasing by 0.0178 percentage points for every tenured professor
  #And the intercept means for universities with no tenured professors the model predicts a retention rate of 73.5%
  #It is important to note that the slope and intercept both have significant p-values at <0.0000000000000002 ***
  #lmTenure also tells us that 738 observations were deleted due to missingness
  #Our residual standard error is 9.295
  #Lastly our R-sq value for the model is .2248, 
  #meaning that this model (Tenured) is able to explain 22.5% of the variation in the data (FTRetentionRate)
#describe visually with plots of model and regression
#Test Assumptions
  #Linearity - plot x and y
plot(Tenured, FTRetentionRate)
abline(lmTenure, col = "red")
    #While the line does capture a good amount of the data, 
    #it is clear that most of the observations are clustered around 0-500 tenured professors,
    #with a good number of observations quite far from the regression line
    #this data would likely better be modeled with a log of Tenured
  #Normalized Residuals - Normal or Skewed
hist(lmTenure$residuals)
mean(lmTenure$residuals)
    #The histogram of the residuals is centered around zero (mean: -0.000000000000002219908),
    #however it does appear slightly skewed left, 
    #likely due to those early observations well below the regression line  we mentioned earlier
  #Homoskedasticity - BPTest
lmtest::bptest(FTRetentionRate ~ Tenured)
    #The Breush-Pagan test gives us a p-value of 0.005616,
    #meaning that the model is likely heteroskedastic, failing this assumption
  #Remaining assumptions with plot of lmTenure
plot(lmTenure)
#Explanation of model in context
    
###VARIABLE 3: Total Faculty
#Prediction of what we will get based on the relationship (pos/neg;strong,weak,moderate) when put in context
#actually create model
#discuss model (R^2, p-value, slope)
#describe visually with plots of model and regression
#Test Assumptions
#Linearity - plot x and y
#Normalized Residuals - Normal or Skewed
#Homoskedasticity - BPTest
#Remaining assumptions with plot of lm
