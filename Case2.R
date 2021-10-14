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

#Given the positive correlation of Avg Salary to FT Retention rate, it implies that as Avg Salary increases, FT retention increases. 
#The reason Professors are generously compensated than their peers, could be due to their relatively better teaching skillset, which adds value to students' learning experience, resulting in retaining students.


#actually create model
lmAvgSal <- lm(FTRetentionRate~AverageSalary, data=Case2)

#discuss model (R^2, p-value, slope)
summary(lmAvgSal)
#Equation: FTRetentionRate = 54.72604970 + 0.00027742 * AverageSalary
#RESIDUALS: 
#The residual summary statistics give information about the symmetry of the residual distribution.
#The variance (i.e. spread) of the residuals decreases as the predicted values increase.
#The quartiles represent that the distribution of the residuals isn't strongly symmetrical. That means the model may have outliers.  

#COEFFICIENT: 
#Intercept: At an average Salary of 0, full-Time Retention is 54.72%.
#Slope: A value of 0.00027742 means 10,000 unit change($10,000 increase) in Average Salary, changes(increases) FT Retention Rate by 2.7742 percentage points

#Residual standard error is a measure of variation of observations around the regression line. Or the average amount that the response (FTRetentionRate) will deviate from the true regression line. 
#A value of 10.69 means the FT Retention Rate can deviate from the true regression line by approximately 10.69, on average.

#Multiple R-squared:  0.2602,	Adjusted R-squared:  0.2597. This statistic measures how well the model fits the actual data.
#The values mean that 26% of the variance found in the response variable (FTRetention Rate) can be explained by Average Salary(predictor variable).

#F-test and p=value: 
#Our model is significant through the F-test value of 568.3 and p=value of 0.00000000000000022

#Test Assumptions
#Linearity - plot x and y
plot(FTRetentionRate~AverageSalary, data=Case2)
abline(lmAvgSal, col="red")
#The line captures a good amount of the data, it is clear that most of the observations are clustered around $50,000-$100,000 Average Salary.
#A lot of observations are quite far from the regression line.


#Normalized Residuals - Normal or Skewed
hist(lmAvgSal$residuals) 
#Slight skewness to the left, overall the residual errors seem normal as they are centered around 0.
mean(lmAvgSal$residuals) #confirms the errors are approximately normal as they average out to 0.

#Homoskedasticity - BPTest
lmtest::bptest(lmAvgSal)
#The Breush-Pagan test gives us a p-value of 0.00000000002458,
#meaning that the model is likely heteroskedastic, failing this assumption

#Remaining assumptions with plot of lm
par(mfrow=c(2,2)) #To print four plots on one page.
plot(lmAvgSal) 
#Residual vs Fitted: 
#We can see heteroskedasticty, as there is a bottleneck pattern in the residual error.
#Aside from a few potential outliers, the model has smaller residual error in the later end(higher Average Salary), than former(lower Avg Salary), demonstrated by the spread of the residuals as the fitted values increase.
#A few residuals stand out from the basic random pattern of residuals. This suggests that there are outliers.

#Normal Q-Q:
#Our plot has a straight line in the center but heavy tails on both end. This may be because we have extreme residuals pulling away with higher standard deviations. 

#Residuals vs Leverage
#Cook's distance measures the effect of deleting a point on the combined parameter vector. Points outside the dotted line have high influence, which in this case there appear to be a few qualifiers.

par(mfrow=c(1,1)) #Returning it to normal viewing condition.



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
  
  #Plotting residuals and leverage shows that there are a number of observations we might consider as outliers,
  #But none breaching Cook's distance line
  #However, as mentioned before, given the low quality of fit for this model, 
  #I would recommend changing the model altogether rather than just removing a few observations

#Explanation of model in context

  #Ultimately this model tells us that increasing the number of Tenured professors will increase retention rate
  #However, this model does not have the best fit, and using log(Tenured) would likely better capture the shape
  #of Tenured's effect on FTRetentionRate and thereby be a better predictor
    
###VARIABLE 3: Total Faculty
#Prediction of what we will get based on the relationship (pos/neg;strong,weak,moderate) when put in context

#Given the correlation of Total Faculty was about .4 (positive, moderate) with FTRetentionRate, 
#we should expect to see the model to have a positive relationship between these variables as well

#linear model assigned to a variable
lmTotalFaculty <- lm(FTRetentionRate ~ TotalFaculty, data=Case2)


#discuss model (R^2, p-value, slope)
summary(lmTotalFaculty)
coef(lmTotalFaculty)

#Equation: FTlmTotalFaculty = 72.705429781 + 0.007367571 * TotalFaculty

#There is a positive relationship between these variables, with Retention increasing by 0.0073
#percentage points for every university's total faculty
#The intercept lets us know that university with no faculty will have a retention rate of 72.7%
#The intercept slope and intercept both have significant p-values at <0.0000000000000002
#which may indicate that total faculty can be a predictor since it is less than 0.05

#lmTotalFaculty notes that  that 390 observations were deleted due to missingness
#Our residual standard error is 11.28, meaning the deviation from true value
#of retention could be off by 11.28
#this number would be useful to compare with RSE of a model
#using log(TotalFaculty) to determine which has the better fit

#Lastly our R-sq value is 0.1385, meaning that this model is able to explain 13.85% of the variation in the data

#describe visually with plots of model and regression
#Test Assumptions

#Linearity- plot x and y
par(mfrow=c(1,1))
plot(TotalFaculty, FTRetentionRate)
abline(lmTotalFaculty, col = "red")

##While the line does capture a good amount of the data, 
#it is clear that most of the observations are clustered around 0-1000 faculty
#members, 
#with a good number of observations quite far from the regression line
#this data would likely better be modeled with a log of TotalFaculty

##This makes sense, as resources and finances are limited in university, hence
#schools would have as limiting number of faculty as possible to keep the salary 
#expenses under control. Hence most of the total faculty is packed between the 
#0-1000. However, there are several points that deviate from the line and have
# a larger amount of faculty with varying retention rate. 

#Normalized Residuals - Normal or Skewed
hist(lmTotalFaculty$residuals)
mean(lmTotalFaculty$residuals)

#The histogram of residuals is centered around 0 (mean: 2.650859e-16)
#however, the graph does #appear to be skewed left which is likely because
#those early observations well below the regression line

#Homoskedasticity - BPTest
lmtest::bptest(FTRetentionRate ~ TotalFaculty)

##The Breush-Pagan test gives us a p-value of 0.2882,
#clearly showing that they  the model is likely heteroskedastic, 
#failing this assumption

#Remaining assumptions with plot of lmTotalFaculty
par(mfrow=c(2,2))
plot(lmTotalFaculty)

#By plotting the residuals with the fitted values, we first see that
#there are both residuals that are positive and negative but 
#with a much larger skew towards negative
#In fact, there are a great deal of negative residuals at the start of 
#the data that are very far from the model
#What this all implies is that the data is not linear, and we have a great
#deal of potential outliers

#By looking at the  Normal Q-Q plot, we see that there are 
#several observations pulling away from the line.
#These negative residuals are over 3 standard deviations from the model, 
#while the positive residuals tend to follow the line and are no more than
#3 standard deviations from the line

#The plot of the fitted values with the square root of standardized residuals
# shows us a somewhat bottleneck pattern in the data as the observations get 
#less spread out. This again proves that the data is heteroskedastic and not random.

#Although we see multiple outliers in residuals vs leverage graph, none of them cross the Cook's distance. 

#A recommendation to see the relationship better between these variables is to 
# change the model altogether instead of ommitting observations. 



#Explanation of model in context
#The linear model tells us that increasing the number of total faculty
#will increase retention rate. However, this model does not have the best fit. 
#Therefore, it is recommended to use the log(Tenured) to better capture the
#relationship. 

#Why do we care?
#Education is a big part of society and higher education is a vastly 
#influencing part of the global environment. Hence, it is crucial for us
#to see what factors to gauge or alter so that the educational institutes 
#can have a sustainable system while still giving a good education to its students.
#Having the right amount of faculty will definitely aid students, likely helping 
#universities reach optimal graduation retention rates. Too little of faculty will
#spread proper supervision too thin, thus decreasing the education quality for students. 
#However, unnecessarily large faculties will be a grave and inefficient expense for 
#universities, which will decrease education institutes stability and sustainability. 

