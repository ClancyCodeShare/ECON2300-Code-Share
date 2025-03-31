install.packages("readr")
install.packages("dplyr")
install.packages("estimatr")
install.packages("psych")

library(readr) #for reading our data quickly and more reliably, using read_csv
library(dplyr) #package for data manipulation
library(estimatr) #pack for lm_robust, allowing us to estimate regressions with
#robust standard errors
library(psych) #Many useful functions for data analysis

#Same basic housekeeping to start this document, we will remove our pre-existing
#environment, and set our working directory:

rm(list= ls())
getwd()
setwd("Users/username/Tutorials/Tutorial 1 Code") #this is just an example, you
#will need to set your own working directory to the appropriate folder


#Question 1; Use the birthweight_smoking.csv introduced in tutorial 3 to answer
#the following

#Before we do anything answer any of the questions, we will need to get the data
#into our environment:

BW = read_csv("birthweight_smoking.csv")

#We will attach the data to avoid having to type BW${variable name} to access
#the columns inside the data

#(a) Regress birthweight on smoker:

reg1 = lm_robust(birthweight ~ smoker, data = BW, se_type = "stata")
summary(reg1)

#(b) Regress birthweight on smoker, alcohol and nprevisit:

reg2 = lm_robust(birthweight ~ smoker + alcohol + nprevist, 
                 data = BW, se_type = "stata")
summary(reg2)

#Notice that the value on the slope coefficient of smoker is less negative 
#(smaller in absolute value), what might this be telling us? Consider this
#before reading the next part.

#(i)
#Using the two conditions for omittmed variable bias, explain why the exclusion
#of alcohol and nprevist could lead to omitted variable bias in the regression
#estimated in (a)

#The two conditions for omitted variable bias to exist in a regression are:

#There is some variable that is not present in the regression that:
  #1: is correlated with at least one of the independent variables in the regression
  #2: has an affect on the dependent variable

#In this case, it is plausible both that alcohol and nprevist would be correlated
#with smoker and affect the baby's birthweight. Hence their omission from reg1
#is a likely source of omitted variable bias.

#(ii) reg2 has a substantially smaller (in absolute value) coefficient on the 
#variable smoker than than reg1, at -217.58 rather than -253.23. This suggests
#the presence of omitted variable bias in reg1, as by including additional
#variables, the coefficient on smoker was substantially changed. It appears
#that the smoker variable was capturing some of the negative effect on birthweight
#due to other variables that happen to correlate with smoker.

#(iii)
#The regression gives us an estimate of the expected value of birthweight,
#given the value of smoker, alcohol and nprevist, so we can say:

#E[birthweight| smoker = 1, alcohol = 0, nprevist = 8] is estimated to be:
#B_0 + B_1smoker +B_2alcohol + B_3nprevist
#= 3051.25 -217.58(1) -30.49(0) +34.07(8)
#= 3106.23 grams
#We could also ask RStudio to calculate this for us, using the predict() 
#function:

predict(reg2, newdata = data.frame(smoker = 1, alcohol = 0, nprevist = 8))
# = 3106.228 grams

#The variable "newdata" allows us to specify the values of the independent
#variables that we want a prediction for. We input the newdata as a data.frame(),
#which is a type of data object in RStudio similar to a matrix. In this case
#we have only input one row of variables, but we could have input more to
#generate predictions for different variable values.

#(iv)

#The value of "multiple R^2" and "adjusted R^2" are very similar because
#the sample size is large, relative to the number of independent variables of the
#regression.

#(c)

#An alternative way to control for prenatal vistits is to use the binary variable
#tripre0 through tripre3. Regress birthweight on smoker, alcohol, tripre0, tripre2
#and tripre3.

reg3 = lm_robust(birthweight ~ tripre0 + tripre2 + tripre3,
                 data = BW, se_type = "stata")
summary(reg3)

#(i)

#Why is tripre1 excluded from the regression? What would happen if you included
#it in the regression?

#tripre1 is excluded from the data to avoid multicolinearity: tripre0 + tripre1
# + tripre2 + tripre 3 = 1. Note that we can still interpret a relationship
#between tripre1 and birthweight, as setting tripre0, tripre2 and tripre3 to 0
#is equivalent to setting tripre1 to 1.

#Including tripre1 leads to a warning message and very high standard errors:

lm_robust(birthweight ~ tripre0 + tripre1 + tripre2 + tripre3,
          data = BW, se_type = "stata")

#(ii)

#tripre0 is a dummy variable, when it is equal to 1, it means the mother did
#not attend any prenatal visits. The coefficient on this term measures the 
#expected difference in birthweight between when tripre0 = 1 and tripre0 = 0,
#other things being equal. Since tripre0 = 1 only when tripre 2 and tripre3 are
#0, this is also the expected difference in birthweight between when tripre0 = 1
#and when tripre1 = 1. tripre1 = 1 corresponds to a mother attending a prenatal
#visit in the first trimester. The large negative value suggests that birthweight
#is expected to be substantially lower when no prenatal checkups are attended,
#relative to the case where a prenatal checkup is attended in the first trimester.

#(iii)

#Just as with tripre0, the coefficients on tripre2 and tripre3 represent the
#expected difference between between birthweight when tripre2 = 1 or when tripre3 = 1,
#with respect to tripre1 being equal to 1.

#(iv) Does the reg3 explain more of the variance in birth weight than reg2?

#No, reg3 has a smaller R^2 variable, so reg2 explains more of the variance 
#in birthweight than reg3.

#Question 2

#Using the dataset Growth.csv, but excluding the data for Malta, run a
#regression for growth on tradeshare

Growth = read_csv("Growth.csv") %>%
  filter(country_name != "Malta")

attach(Growth)

#This is yet another way of excluding Malta from the dataset, using functionality
#from dplyr to filter according to the condition that the country name is not
#equal to Malta.

#(a) Construct a table that shows the sample mean, standard deviation,
# and minimum and maximum values for the series: growth, tradeshare,
#yearschool, oil, rev_coups, assassinations, and rgdp60.

#These are all descriptive statistics that can be accessed with
#the command "describe()"

describe(data.frame(growth, tradeshare, yearsschool, oil, rev_coups,
                   assasinations, rgdp60))

#(b) Run a regression of growth on tradeshare, yearsschool, oil,
#rev_coups, assasinations and rgdp60.  Use the regression to predict the average
#annual growth rate for a country that has average values for all regressors

reg4 = lm_robust(growth ~ tradeshare + yearsschool + oil + rev_coups +
                   assasinations + rgdp60, data = Growth, se_type = "stata")
summary(reg4)

predict(reg4, newdata = data.frame(tradeshare = mean(tradeshare),
                                   yearsschool = mean(yearsschool),
                                   oil = mean(oil),
                                   rev_coups = mean(rev_coups),
                                   assasinations = mean(assasinations),
                                   rgdp60 = mean(rgdp60)))

#For the newdata, simply input the mean of each variable

#(c) Repeat part b, but assume that the country's value for tradeshare is 
#one standard deviation above the mean

#Here we will use the same regression but when using predict, will
#will input mean(tradeshare), plus the standard deviation of tradeshare.

predict(reg4, newdata = data.frame(tradeshare = mean(tradeshare) + sd(tradeshare),
                                   yearsschool = mean(yearsschool),
                                   oil = mean(oil),
                                   rev_coups = mean(rev_coups),
                                   assasinations = mean(assasinations),
                                   rgdp60 = mean(rgdp60)))

#(d)Why is oil omitted from the regression? What would happen if it were included?

#The variable for oil is true for all countries in the sample, so it would
#always be set to 1 and create a perfect multicolinearity problems.