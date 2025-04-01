#As usual, clear your environment and set your working directory:

rm(list=ls())

getwd()
setwd("[Your working directory here]")

#Then access all of the packages we will need for this tutorial:

#Install any that you haven't already:

install.packages("readr")
install.packages("dplyr")
install.packages("estimatr")
install.packages("texreg")
install.packages("car")

library(readr) # package for fast read rectangular data
library(dplyr) # package for data manipulation
library(estimatr) # package for common estimators with robust SE (like lm_robust)
library(texreg) #package for converting R regression to LaTeX/HTML tables
library(car) # package for functions used in "An R companion to Applied Regression"

#Question 1
#Using the Birthweight_Smoking.csv introduced in earlier tutorials, answer the
#following:


#Before attempting any of the questions, we should load the data into our
#environment:

BW = read_csv("birthweight_smoking.csv")

#And attach the data to save time later:

attach(BW)
#Run a regression of birthweight on:
#(1) smoker
#(2) smoker, alcohol and nprevist
#(3) smoker, alcohol, nprevist, and unmarried

reg1= lm_robust(birthweight ~ smoker, 
                data = BW, se_type="stata")

reg2= lm_robust(birthweight ~ smoker + alcohol + nprevist, 
                data = BW, se_type="stata")

reg3= lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried, 
                data = BW, se_type="stata")

#To save all of this data into a neat, formatted table, copy the following
#into you console. We don't want to have it run as part of our R file because
#the output will just unnecessarily clog up our console.

#texreg(list(reg1,reg2,reg3), include.ci = F, caption.above = T)

#The first input is the list of regressions we are interested in,
#the second tells it not to include confidence intervals (we'll calculate
#these ourselves later)  and the third tells it to put captions on the top
#rather than bottom of the table.

#The output of this command is a table written in LaTeX that we can read in
#an R Markdown file.

#To do this, first install the package "tinytex"and activate it the same way
#we do with other packages.

#Then copy the output of the texreg command from early.

#You can then input this into anything that read latex and get our table.

#One option is to use an online resource like Overleaf. On Overleaf, you can
#create a blank document and paste the texreg out put between the \begin{document}
#and \end{document} lines, then press recompile to get your table.

#Alternatively, you can make the table inside of RStudio by install R Markdown
#and tinytex.

#You can install R Markdown by going to File -> New File -> RMarkdown inside
#RStudio and clicking "yes" when prompted about whether to install additional
#packages. Alternatively, type "install.packages("rmarkdown") into your console.

#Then install tiny text and activate it with install.packages("tinytext"), then
#library(tinytex) and finally, this last one will take a while:
#tinytex::install.tinytex()

#Once this has run, you can open an RMarkdown file and past the texreg output
#inside, then press the Knit button and select "Knit to PDF." The PDF should
#contain your table.



#(a) What is the value of the estimated effect of smoking on birth weight in 
#each of the regressions?

#This is given by the slope coefficient on the smoking variable, you can consult
#the regression summaries or the table we produced for this:

#(1) -253.23g
#(2) -217.58g
#(3) -175.38g

#(b) Construct a 95% confidence interval for the effect of smoking on birth
#weight, using each of the regressions.

#You should still recall how to hand calculate these using the formula:
#CI = estimate +/- T * stderror

#Here we will use RStudio to access these from our regression:
reg1$conf.high["smoker"]
reg1$conf.low["smoker"]

reg2$conf.high["smoker"]
reg2$conf.low["smoker"]

reg3$conf.high["smoker"]
reg3$conf.low["smoker"]

#Alternatively we could just look at the summaries of each of these. This was
#just to show you we can access different parts of our regressions more directly.

#(c) Does the coefficient on smoker in regression (1) suffer from omitted variable bias?

#It appears so, the slope coefficient on smoker decreases substantially when
#some plausible omitted variables are included in regressions (2) and (3).

#(d) Does the coefficient on smoker  in regression (2) suffer from omitted variable bias?

#Again, it seems so, as the coefficient decreases substantially in regression (3)
#when plausbile omitted variabled are included.

#(e) Consider the coefficient on unmarried in regression (3):

#(i) construct a 95% confidence interval for the coefficient

reg3$conf.low["unmarried"]
reg3$conf.high["unmarried"]

#(ii) Is the coefficient statistically significant?

#Yes, we can observe the estimated p value or t statistic to see this, or we 
#can see that our confidence interval does not contain 0 so we can reject the
#null hypothesis, making the coefficient on "unmarried" statistically significant

#(iii) Is the magnitude of the coefficient large?

#Yes, an estimated difference or 187g is substantial, this represents about 5%
#of the mean birthweight.

#(iv) As the question suggests, "unmarried" is a control variable that is
#accounting for the impact of other factors that have not been included in the
#regression. Obviously, being married itself cannot have any impact on a baby's
#birthweight, but there may be differences on average between married and
#unmarried mothers that do have an impact on birthweight, and this is supported
#by the substantial, significant coefficient on the "unmarried" variable.

#(f) Consider the various control variables included in the data set. Which do
#you think should be included in the regression? What is a reasonable 95%
#confidence interval for the effect of smoking on birth weight?

#All the way up to model (3) we have found substantial ommitted variable bias,
#if we go a step further and add educ and age into the regression, we see there
#is not a big difference compared to (3), so we can say regression (3) is
#probably robust enough, at least with the data we have on hand.

reg4 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried + educ + age,
                 data = BW, se_type="stata")
summary(reg4)

#Question 2

#First lets clear out our environment again:

rm(list=ls())

#(a) Suppose that the mechanism described in question 2 is correct. Explain
#how this leads to ommited variable bias in the OLS regression of earnings on
#height. Does the bias lead the estimated slope to be too large or too small? 

#The proposed relationship would mean that childhood nutrition correlates with
#height and determines cognitive ability, which affects lifetime earnings. If
#this is assumed to be true, the coefficient on height would be overestimated,
#as increased height would be associated with higher earnings, but it would
#only be picking up the positive impact of early childhood nutrition, which,
#according to the proposed relationship, increases both height and earnings.

#Next we will construct dummy variables from the educ variable to attempt a test
#for the proposed relationship.

#Note we have not loaded our dataset into the library yet, because we want
#to add in our dummy variables as we do so.

EH = read_csv("Earnings_and_Height.csv") %>%
  mutate(lt_hs = as.numeric(educ<12), hs = as.numeric(educ == 12), 
         col = as.numeric(educ>=16), some_col = 1 - lt_hs - hs - col)
attach(EH)  

#Using the mutate command from dplyr allows us to create new variables, based
#on the preexisting onces. Here we set the dummy variables equal to 1 or 0 based
#on whether the stated condition (eg educ<12) is true or false.

#We make our dummy variables exhaust all possibilities by making som_col equal
#to 1 unless one of the other dummy variables is turned on.

#Note there are many other ways that we could have done this. For example we can
#access a list of 1's and 0's based on educ just by typing the condition:

as.numeric(educ<12)

#We could then add this as a column to our data
EH = data.frame(EH, as.numeric(educ<12))

#and edit the column name to whatever we like by accessing the column names:

colnames(EH)

#and changing the desired name using indexing:

colnames(EH)[16] = "REDUNDANT lt_hs"

#I chose this particular name to note that this column is the exact same as
#lt_hs

#We could also just create new variables:

lt_hs2 = as.numeric(educ<12)

#doing this for each of the required dummy variables.

#Using the mutate command is probably the neatest approach, but its important
#to recognise that, as is often the case, its not the only way.

#After that digression, we we run the regressions asked for by the question:

reg1 = lm_robust(earnings ~ height,
                 data = subset(EH, sex == "0:female"),
                 se_type = "stata")
reg2 = lm_robust(earnings ~ height + lt_hs + hs + some_col,
                 data = subset(EH, sex == "0:female"), 
                 se_type = "stata")

reg3 = lm_robust(earnings ~ height,
                 data = subset(EH, sex != "0:female"),
                 se_type = "stata") 
reg4 = lm_robust(earnings ~ height + lt_hs + hs + some_col,
                 data = subset(EH, sex != "0:female"), 
                 se_type = "stata")

#The first 2 regressions only use female data, and the second 2 only use male
#data.

#(b)

#(i) Compare the estimated coefficients on height in regressions (1) and (2).

summary(reg1)

summary(reg2)

#There is a BIG difference in the height coefficient, going down roughly 80%
#from reg1 to reg2. Technically we cannot call it significant yet though
#as we need to run a proper hypothesis test. This is not a rigourous hypothesis
#test, but it certainly supports the idea that reg1 has ommited variable bias.

#(ii) Why does reg2 omit col?

#This would lead to a perfect multicolinearity problem, giving us no solution
#for the regression.

#(iii) Test the joint null hypothesis that the coefficients on education
#variables are equal to 0. We can do this using the linearHypothesis command:

linearHypothesis(reg2, c("lt_hs=0", "hs=0", "some_col=0"), test=c("F"))

#This returns a high F statistics and a very small p value, meaning we have
#joint statistical significance at at least the 5% significance level. 

#This means we reject the null hypothesis that all the education dummy variables
#are equal to 0. We could say in other words that there is a statistically
#significant difference between the college group and at least one of the other
#groups.

#(iv) Discuss the estimated coeficients on the dummy variables.

#All of the coefficients are negative, implying that the expected effect of 
#going to to college, meaning setting none of the dummy variables to 1, is positive.
#This makes sense, as college is the highest education level among the dummy
#variables.

#Additionally, the coefficients becomes less negative as the education level
#becomes higher, reflecting the expected relationship that education increases
#earnings

#(c) We get mostly the same results when comparing the regressions for males.

#(i) 

summary(reg3)
summary(reg4)

#again, the coefficient on height falls substantially, about 50%, between the
#two regressions.

#(ii)

#Again, multi-colinearity

#(iii)

linearHypothesis(reg4, c("lt_hs=0", "hs=0", "some_col=0"), test=c("F"))

#Again, we have statistical significance


#(iv)

#The same relationship appears where increasing levels of education predict
#increased earnings