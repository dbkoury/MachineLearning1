###TEAM NAME: Team 2
##TeamMates: Riti Dabas, Dylan Koury, Anoushka Mahar

#importing packages and prep
options(scipen=999)

#Getting the Data
setwd("C:/Users/dylan/MSBA/ML1/Assignments")
Case2 <- read.csv("Case2.csv")

#Looking at the Data
str(Case2)
summary(Case2)

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
#actually create model
#discuss model (R^2, p-value, slope)
#describe visually with plots of model and regression
#Test Assumptions
#Linearity - plot x and y
#Normalized Residuals - Normal or Skewed
#Homoskedasticity - BPTest
#Remaining assumptions with plot of lm
    
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
