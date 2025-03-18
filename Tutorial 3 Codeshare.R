rm(list = ls())
setwd("[Your working directory here]")
#If you don't know how to find your working directory, its the file path
#that leads to the folder where your data is stored.
#To access this, go to the folder you are looking for, then if using windows
# you can right click the file path shown at the top of the window in File
#Explorer, while you have to folder open, and select copy as text. 
#You will need to replace the "\" symbol with "/"

#If on Mac, also go to the folder you want to set as your working directory,
#then right-click the folder and hold down the option key. While holding the 
#option key, click "copy "filename" as "pathname." If the pathname uses the
#"\" symbol, replace it with "/"
getwd()

#Install necessary packages, if you haven't already
install.packages("readr")
install.packages("dplyr")
install.packages("estimatr")
install.packages("psych")

#Load the necessary packages:
library(readr) # package for fast read rectangular data
library(dplyr) # package for data manipulation
library(estimatr) # package for commonly used estimators with robust SE
library(psych) # package containing many functions useful for data analysis



#Question 1

#The file Earnings and Height.csv contains data on earnings, height, and other 
#characteristics of a random sample of U.S. workers. See Earnings and Height 
#Description.pdf for a detailed description of the data. Carry out the 
#following exercises.

#First, lets get the data into our working directory. Note we are using
#read_csv instead of read.csv from last tutorial. This is a command from readr
#that just has some additional functionality.

EH = read_csv("Earnings_and_Height.csv")
#And, after looking at the description pdf, have a look at the data:
View(EH)

#Finally, attach the data so we don't need to refer back to it when
#specifying our variables (eg we can just write "earnings" rather than
#earnings$EH). You can detach it using detach()
#NOTE: because we specify our data inside of lm_robust, you can write your
#variable names like this without needing the data attached.

attach(EH)

#(a) Run a regression of earnings on height.

#Use lm_robust to ensure our standard errors are robust to economic data
#By setting se_type to "stata" we use heteroskedasticity robust standard errors

reg1 = lm_robust(earnings~height, data = EH, se_type = "stata")

#Use summary to see several important estimates from your regression:
summary(reg1)

#(a)(i) Is the estimated slope statistically significant?
#By this we mean, we want to check if the slope is statistically significantly
#different to 0.
#You can check any of the following to check this:

#t value: 14.0425
#This is a large t statistic, a lot bigger than 1.96 which implies statistical
#significance at at least the 95% confidence level, or 5% significance level.

#p value (in summary this is Pr(>|t|)): 1.478e-44
#That is another way of writing 1.478*10^-44. This is a very, very small number,
#a lot smaller than p=5%, (and 1% and 0.1% and so on for that matter) so we
#can reject the null hypothesis at the p=5% significance level. 
#This result can be interpreted as: if the population really did have a slope
#coefficient of 0, the chance that our estimated coefficient would be at least
#as far from 0 as it is, is 1.478*10^-44. Very, very unlikely.

#Confidence interval: Lower bound = 608.9, Upper bound = 806.5
#The confidence interval is by default a 95% confidence interval (you can
#change this by specifying a different "alpha" value in the regression,
#check the documentation in ?lm_robust() if you want to see how). Since this
#interval does not cross 0, we can reject the null hypothesis at the 95%
#confidence level, or 5% significance level.

#(a)(ii) Construct a 95% confidence interval for the slope coefficient.

#lm_robust gives us a confidence interval but it is still important you
#know how it is calculated.

#Given the estimated slope of 707.7 and Standard error of 50.4, and using
#a t value of 1.96 for a 95% confidence interval, the calculation is:

#confidence interval = slope +/- (standard error) * 1.96
#confidence interval = 707.7 +/- 50.4*1.96
#confidence interval = (608.9, 806.5)
#Check that you get the same result as the summary statistics states.

#(b) Repeat (a) for female observations

#We can perform the same regression for female observations by restricting
#our data set accordingly.

reg2 = lm_robust(earnings~height, 
                 data = subset(EH, sex != "1:male"),
                 se_type = "stata")

#The subset command lets us retrieve a subset of our data, under specified
#conditions. In this case, we specfied that the value in the "sex" column
#not be "1:male". See that != is the symbol for "not equal to."

#Note, there are many ways we could have restricted our data to only include
#female observations. We could have written == "1:female" to include only
#females rather than excluding males. We could also have used indexing:

EH[EH[,1] == "0:female",]

#This retrieves only the rows from EH in which the first column is equal to
#"0:female"
#You don't need to know every possible way to do this, just be aware
#that there are multiple approaches.

#The rest of the question is approached the same as in part (a):

summary(reg2)

#See that the estimated slope is now smaller. How can you interpret this result?
#Consulting any of the three values: t value, p value, or confidence interval,
#shows that this result is also significant at the 5% significance level.

#t value = 5.239                       > 1.96
#PR(>|t|) = 1.650*10^-7                < 0.05
#Confidence interval = (319.9, 702.5)  Does not contain 0.

#(c) Repeat part (a) for male observations

#We can copy our code from part (b) but this time specify that "sex == 1:male"
#so that sex equals male:

reg3 = lm_robust(earnings~height, 
                 data = subset(EH, sex == "1:male"),
                 se_type = "stata")
summary(reg3)

#Our estimated coefficient is now higher than both previous regressions.
#Again, how might we interpret this result?
#Checking t, p and confidence interval values, we see we have statistical
#significance again:

#t value = 13.220                    > 1.96
#PR(>|t|) = 1.771*10^-39             < 0.05
#Confidence interval = (1113, 1501)  Does not contain 0

#(d) Test the null hypothesis that the effect of height on earnings is the same
#for men and women.

#When I asked how you might interpret the different coefficient sizes in parts
#(b) and (c), a reasonable answer would be that "it looks like differences in
#height are associated with bigger differences in income for men than for women"

#This is reasonable because the estimated coefficient represents how much
#more money you expect an individual to make, given a unit increase in height.

#But although the estimates are different, we cannot immediately be confident
#that a difference exists at the population level. Perhaps there is no
#difference at the population level, and our samples show a difference just
#because of random variation. The solution to this problem: use a hypothesis
#test to check if the difference between the coefficients is statistically
#significant. 

#We want to test whether the difference:

#B_1male - B_1female

#Is statistically significant. So let's construct a 95% confidence interval
#around that estimate:

#Confidence interval = 
# (B_1male - B_1female) +/- 1.96 * std.error(B_1male - B_1female)

#We can simply calulate:

#B_1male - B_1female = 1307 - 511.2 = 795.8

difference = 1307 - 511.2

#Then we just need the standard error of this difference. Use the formula
#for this:

#std.error(B_1male - B_1female) =
# sqrt( std.error(B_1male)^2 + std.error(B_1female)^2 )

std.error = sqrt(98.86^2 + 97.58^2) # = 138.907

#Then finally, the confidence interval, we will make it a vector of values
#using c(), where the first value is the lower bound, and the second is the
#upper bound

CI = c(difference - 1.96*std.error, difference + 1.96*std.error)
CI #= (523.5423, 1068.0577)
#Confidence interval does not contain 0, so we have significance at the 5% level
#Hence we can be confident that the coeficients are in fact different, and 
#confident also that height differences are associated with bigger earnings
#differences in males than in females. Still, we should be careful about
#interpreting causal relationships without further analysis.


#Question 2 

#Using the dataset Growth.csv, but excluding the data for Malta, run a 
#regression of growth on tradeshare.

#First, lets clear off what we were doing before:
rm(list = ls())

#Now access the data:

Growth = read_csv("Growth.csv")

#And perform the heteroskedasticity robust linear regression as before, using
#the subset of data that excludes Malta.

reg1 = lm_robust(growth ~ tradeshare, 
                 data = subset(Growth, country_name!="Malta"),
                 se_type = "stata")
summary(reg1)

#(a) Is the estimated regression slope statistically significant? This is,
#can you reject the null hypothesis H0 : β1 = 0 vs. H1 : β1 ̸= 0 at the 5% or
#1% significance level?

#Checking the summary of reg1, we see the following regarding the slope
#coefficient on tradeshare:

#t value = 1.942          < 1.96
#Pr(>|t|) = 0.05670       > 0.05
#CI = (-0.04944, 3.411)   Does contain 0

#All of our tests tell us that we do not have statistical significance at the
#5% level, and hence we also do not have statistical significance at the 1%
#level.

#(b)

#The p value associated with the t statistic in this circumstance, is the
#probability of getting a slope coefficient result that is at least as far
#from 0 as the one we estimated, under the assumption that the null hypothesis
#holds. That is, the assumption that the slope coefficient for the population
#is in fact 0.

#We can see this in summary statistics, under Pr(>|t|).
#We see that p = 0.05670.

#So, if the population slope coefficient was 0, our model estimates that there
#is a 0.05670 probability of that our sample would be as far as, or further from,
#0 as it was. Note: I say "estimates" because we do not know the precise
#standard error but rather have estimated it from the sample data.

#(c) 

#Construct a 99% confidence interval for β1.

#The t value for 99% confidence with a large sample size is approximately 2.58,
#so calculate the confidence interval as before using the formula:

#CI = B_1 +/- 2.58 * std.error
#CI = 1.68 +/- 2.58 * 0.87
#CI = (-0.56 , 3.93)

#Note that the interval contains 0, hence not statistically significiant at 1%
#significance level.

#Question 3

#The data file Birthweight Smoking.csv contains data for a random sample of
#babies in Pennsylvania in 1989. The data include the baby’s birth weight
#together with various characteristics of the mother, including whether she 
#smoked during the pregnancy. See Birthweight Smoking Description.pdf
#for a detailed description of the data. You will investigate the relationship 
#between birth weight and smoking during pregnancy.

#First remove what we were doing before:

rm(list= ls())

#Now access the data:

BW = read_csv("Birthweight_Smoking.csv")
View(BW)

#In this question have to access some of the columns in birthweight, it will
#therefore be convenient to attach the data set as we will be able to write
#the column names without using $BW in order to tell rStudio where to find
#the data.

attach(BW)

#(a) In the sample:

#(i) What is the average value of birthweight?

#Describe is a useful command to retrieve several descriptive statistics:
describe(birthweight)

#Here we could also use:
mean(birthweight)

#And we see the average value is 3382.934

#(ii) What is the average birthweight for mothers who smoke?

#(iii) What is the average birthweight for moother who do not smoke?

#We can tackle these both at the same time using the command "tapply"
#tapply(X, INDEX, FUN) will apply the function, FUN, to the data, X, split
#up based on the index specified in INDEX. So:

tapply(birthweight, smoker, describe)

#Applies describe() first to the subset of birthweight where smoker is 0,
#and the to the subset where smoker is 1.

#These are 3432.06 for smoker = 0 and 3178.83 for smoker = 1

#Alternatively we could take:

mean(birthweight[smoker == 0])
mean(birthweight[smoker == 1])

#This retrieves the mean of birthweight, indexing only to datapoints where
#smoker is equal to either 0 or 1 (depending on whether we set == 1 or == 0).


#(b)

#We have already output everything we need for this calculation in the
#descriptive statistics we accessed in part (a).

#(i)  Use the data in the sample to estimate the difference in average birth
#weight for smoking and non-smoking mothers.

#This is just the difference between the averages we calculated:

#X_smoker - X_nonsmoker = 3178.83 - 3432.06 = -253.23

#(ii) What is the standard error for the estimated difference in (b)i?

#We have to caluclate the standard error for a difference of two means as we
#did earlier. The formula is as before:

#se(X-Y) = sqrt(se(X)^2 + se(Y)^2)

#se(X_smoker - X_nonsmoker) = sqrt(se(X_smoker)^2 + se(X_nonsmoker)^2)

#se(X_smoker - X_nonsmoker) = sqrt(11.89^2 + 24.04^2)

#se(X_smoker - X_nonsmoker) = 26.82

#(iii) Construct a 95% confidence interval for the difference in the average
#birth weight for smoking and nonsmoking mothers

#Using the confidence interval formula with the values we have already
#calculated, and t=1.96 for a 95% confidence interval:

#CI = (X_smoker - X_nonsmoker) +/- 1.96 * se(X_smoker - X_nonsmoker)
#CI = -253.25 +/- 1.96 * 26.82
#CI = (-305.8, -200.66)

#We could also shortcut all of this work (but only do this if you're completely
#confident in the theory underpinning the process we just did) by using t.test

t.test(birthweight[smoker == 1], birthweight[smoker == 0])

#This will conduct a t test on the two sets of data we input, testing the
#hypothesis that the difference between the two is 0. We index birthweight
#to smoker == 1 and smoker == 0 to partition the data into the groups we are
#interested in.

#You can also use this to easily conduct t tests at different confidence levels:
t.test(birthweight[smoker == 1], birthweight[smoker == 0], conf.level = 0.99)


#(c) Run a regression of birthweight on the binary variable smoker:

reg3 = lm_robust(birthweight ~ smoker, data = BW, se_type = "stata")
summary(reg3)


#(i) How are the estimated slope and intercept related to your answers in (a)
# and (b)?

#Notice that the estimate for the slope coefficient is the same as the estimated
#difference between birthweight for smoker = 1 and smoker = 0. This accords with
#the interpretation of the slope coefficient as the difference in the expected
#value of the dependent variable corresponding to a unit increase in the
#independent variable.

#Additionally, notice that the intercept estimate is the same as the mean or
#expected birthweight we calculated with smoker set to 0. This accords with the
#interpretation of the intercept, being the value that the dependent variable
#is expected to take when all other variables are 0.

#(ii) How is the SE(B_1) related to your answer in (b)ii?

#The standard error here is the same as the standard error we calcualted for the
#difference between mean birthweight for smokers and non-smokers. This is to be
#expected, because B_1 can be interpreted as the difference in expected birth
#weight between smokers and non-smokers. We have found the same standard error,
#going down a different path.

#(iii) Construct a 95% confidence interval for the effect of smoking on birth
#weight

#Consulting the summary of our regression, we can construct this confidence
#interval around B_1, using the estimate of B_1, the standard error of B_1,
#and the t value 1.96 for 95% confidence:

#CI = B_1 +/- 1.96 * se(B_1)

#Again, this will give us the same confidence interval as the one we estimated
#in part b:

#CI = -253.2 +/- 1.96 * 26.98
#CI = (-305.9, -200.6)


