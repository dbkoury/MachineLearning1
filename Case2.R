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
  #Our residual standard error is 9.295, meaning the deviation from the true value of retention could be off by 9.295, 
  #this number would be useful to compare with the RSE of a model using log(Tenured) to determine which has the better fit
  
  #Lastly our R-sq value for the model is .2248, 
  #meaning that this model (Tenured) is able to explain 22.5% of the variation in the data (FTRetentionRate)

#describe visually with plots of model and regression
#Test Assumptions

  #Linearity - plot x and y
par(mfrow=c(1,1))
plot(Tenured, FTRetentionRate)
abline(lmTenure, col = "red")

    #While the line does capture a good amount of the data, 
    #it is clear that most of the observations are clustered around 0-500 tenured professors,
    #with a good number of observations quite far from the regression line
    #this data would likely better be modeled with a log of Tenured

    #This makes sense, as more schools would have fewer tenured professors because 
    #getting tenure is a difficult achievement and schools with smaller staff have a ceiling
    #Perhaps measuring tenured professors/number of professors would give a measurement less biased by school size

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
par(mfrow=c(2,2))
plot(lmTenure)

  #By plotting the residuals with the fitted values we can gather a few things,
  #First we see that the second half of residuals are all positive,
  #while the first half of residuals are both positive and negative but with a much larger skew towards negative
  #In fact, there are a great deal of negative residuals at the start of the data that are very far from the model
  #What this all implies is that the data is not linear, and we have a great deal of potential outliers

  #When we look at the Normal Q-Q plot, we can see that there are in fact several observations pulling away from the line,
  #These negative residuals are over 3 standard deviations from the model, 
  #while the positive residuals tend to follow the line and are no more than 3 standard deviations from the line
  
  #The plot of the fitted values with the square root of standardized residuals shows us 
  #there is a bit of a bottleneck pattern in the data as the observations get less spread out
  #This goes to further qualify that this data is heteroskedastic and not random.
  
  #Plotting residuals and leverage shows that there are a number of observations we might consider as outliers
  #However, as mentioned before, given the low quality of fit for this model, 
  #I would recommend changing the model altogether rather than just removing a few observations

#Explanation of model in context

  #Ultimately this model tells us that increasing the number of Tenured professors will increase retention rate
  #However, this model does not have the best fit, and using log(Tenured) would likely better capture the shape
  #of Tenured's effect on FTRetentionRate and thereby be a better predictor
    
###VARIABLE 3: Total Faculty
#setwd("C:/Users/anous/Desktop/Machine Learning/Team Assignment/Case 2")
#Case2 <- read.csv("Case2.csv")
#attach(Case2)
#str(case)
#pairs(case)

#cor(data[c(-1,-2)] , use= "pairwise")

#dependent variable = FT Retention Rate


#Prediction of what we will get based on the relationship (pos/neg;strong,weak,moderate) when put in context
#Given the correlation of Total Faculty was about .4 (positive, moderate) with FTRetentionRate, 
#we should expect to see the model to have a positive relationship between these variables as well


#make variable
lmTotalFaculty <- lm(FTRetentionRate ~ TotalFaculty, data=Case2)


#discuss model (R^2, p-value, slope)
summary(lmTotalFaculty)
coef(lmTotalFaculty)

#Equation: FTlmTotalFaculty = 72.705429781 + 0.007367571 * TotalFaculty
#There is a positive relationship between these variables, with Retention increasing by 0.0073
#percentage points for every university's total faculty

#The intercept lets us know that university with no faculty will have  retention rate of 72.7%
#It is important to note that the slope and intercept both have significant p-values at <0.0000000000000002
#which may indicate that total faculty can be a predictor since it is less than 0.05

#it should be noted that 390 observations were deleted due to missingness

#To-Do-List: start from here
par(mfrow=c(1,1))
plot(TotalFaculty, FTRetentionRate)
abline(lmTotalFaculty, col = "blue")

#To-Do-List: may violate one of the assumptions (probably skewed), see residual plot and confer to data background if possible (similar to swirl exercise)
hist(TotalFaculty)
mean(TotalFaculty)
lmtest::bptest(FTRetentionRate ~ TotalFaculty)

#Prediction of what we will get based on the relationship (pos/neg;strong,weak,moderate) when put in context
#discuss model (R^2, p-value, slope), #describe visually with plots of model and regression
#Test Assumptions
#Linearity - plot x and y, #Normalized Residuals - Normal or Skewed, #Homoskedasticity - BPTest, #Remaining assumptions with plot of lm
