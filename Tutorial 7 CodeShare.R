#The packages for this tutorial are as follows, and have all been used
#previously in the tutorials

library(readr)
library(dplyr)
library(ggplot2)
library(estimatr)
library(texreg)
library(car)
library(multcomp)

#Set your working directory:

getwd()
setwd(["your working directory here"])

#Question 1:

#We will need access to the CPS12 data from the previous tutorial, we also want
#to mutate it with the same variables we added in last time, because we will
#use many of the regressions from the previous tutorial. We also need
#to add several interaction terms of the age and age squared variables with
#the male and female variables.

CPS12 = read_csv("cps12.csv") %>%
  mutate(ln_ahe = log(ahe),
         ln_age = log(age),
         age2 = age*age,
         fem_bac = female*bachelor,
         fem_age = female*age,
         fem_age2 = female*age2,
         bac_age = bachelor*age,
         bac_age2 = bachelor*age2)

#We will now run all of the regressions we need, those from (b), (c) and (d), as
#well as 

reg1 = lm_robust(ahe ~ age + female + bachelor, data =CPS12,
                 se_type = "stata")
reg2 = lm_robust(ln_ahe ~ age + female + bachelor, data =CPS12,
                 se_type = "stata")
reg3 = lm_robust(ln_ahe ~ ln_age + female + bachelor, data =CPS12,
                 se_type = "stata")
reg4 = lm_robust(ln_ahe ~ age + age2 + female + bachelor, data =CPS12,
                 se_type = "stata")
reg5 = lm_robust(ln_ahe ~ age + age2 + female + bachelor + fem_bac, data =CPS12,
                 se_type = "stata")
reg6 = lm_robust(ln_ahe ~ age + age2 + fem_age + fem_age2 + female + bachelor +
                   fem_bac, data = CPS12,
                 se_type = "stata")
reg7 = lm_robust(ln_ahe ~ age + age2 + bac_age + bac_age2 + female +
                   bachelor + fem_bac, data =CPS12,
                 se_type = "stata")
reg8 = lm_robust(ln_ahe ~ age + age2 + fem_age + fem_age2
                 + bac_age + bac_age2 + female + bachelor + fem_bac,
                 data = CPS12, se_type = "stata")

#This texreg command puts all of our regressions into a neat table that we can
#refer to.
texreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8),
       include.cv = F, caption.above = T, digits = 3,
       caption = "Earnings and Age 2012",
       custom.model.names = c("(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)"))

#1(h): Plot the regression relation between age and ln(ahe) from (b), (c), (d)
#for males with a high school diploma. Describe the similarities and differences
#between the estimated regression functions. Would your answer change if you
#plotted the regression function for females with a college degree?

#For this question we want to plot age against ln_ahe, with the variables female
#and bachelor set to 0, leaving only age related independent variables. We can
#define each of these:

#We have ages from 25 to 34, so define that with the sequence function seq(),
#specifying the starting value, the ending value and the difference between
#each item in the sequence:
age = seq(25,34, by = 1)

#Then each of the regression relations can be worked out by calcualting our
#regressions for the set of age values we definied:

ln_ageb = reg2$coefficients[1] + reg2$coefficients[2]*age

ln_agec = reg3$coefficients[1] + reg3$coefficients[2]*log(age)

ln_aged = reg4$coefficients[1] + reg4$coefficients[2]* age + 
  reg4$coefficients[3]*age^2

#We can now make each of these into a data frame that contains the age and age
#and corresponding ln_age values:

datab = data.frame(ln_age = ln_ageb, age = age, data = "b")
datac = data.frame(ln_age = ln_agec, age = age, data = "c")
datad = data.frame(ln_age = ln_aged, age = age, data = "d")

#Then we can put these all together using rbind() which will connect our data
#frames together by their rows (whereas cbind will bind them by their columns).
#What I mean by this will make more sense when you observe the data resulting
#dataframe.

data.bcd = rbind.data.frame(datab, datac, datad)

#Now we will use ggplot to plot these all together

#Specifying the x and y variables in aes in the first step means that each
#subsequent usage of aes will use those specified values of x and y

fig8.2 = ggplot(data.bcd, aes(x= age, y = ln_age)) +
  geom_line(aes(col = data), size = 0.8) + #You could stop here and get a sensible 
#looking plot, additional details make it more presentable.'
  labs(title = "Figure 2: Regression Lines (2) - (4)",
       x = "Age", y = "log of AHE") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, size = 12, family = "serif",
                                  face = "bold"),
        legend.key = element_rect(color = "transparent"),
        legend.background = element_rect(fill = "lightgrey",
                                         size = 0.8,
                                         linetype = "solid")) +
  scale_color_discrete(name = "Model", labels = c("Regression (2)",
                                                  "Regression (3)",
                                                  "Regression (4)"))
#Plotting the regression functions for a female with a bachelor degree would
#yield the same shape curves, but shifted vertically such to have a differnt
#intercept value, according to the coefficients on the terms that include
#the female and bachelor variable, which become "activated" when female = 1,
#and bachelor = 1.

#(i) This regression corresponds to reg5 that we ran at the beginning. The
#interaction term shows the "extra effect"of bachelor on ln(ahe) for women,
#relative to the effect for men. It also shows the "extra effect" of female for
#those with bachelor degrees as opposed to high school degrees.

#The predicted values in each case are:

#Alexis:

predict(reg5, newdata = data.frame(age = 30, age2 = 30^2,
                                   female = 1, bachelor = 1, fem_bac = 1))
#Jane:
predict(reg5, newdata = data.frame(age = 30, age2 = 30^2,
                                   female = 1, bachelor = 0, fem_bac = 0))

#Bob:
predict(reg5, newdata = data.frame(age = 30, age2 = 30^2,
                                   female = 0, bachelor = 1, fem_bac = 0))

#Jim:
predict(reg5, newdata = data.frame(age = 30, age2 = 30^2,
                                   female = 0, bachelor = 0, fem_bac = 0))
#see that the difference between male and feel estimates is fairly large,
#but the difference between bachelor and no bachelor within male and female
#is relatively small, which reflects the relative size of the interaction term
#to the female and bachelor variables.

#(j) Here we want to test an interaction term of age and female, to see if there
#is any "extra effect" of age when the female variable is set to 1.
#We will use reg6, which has two interaction terms, fem_age and fem_age2

linearHypothesis(reg6, c("fem_age = 0", "fem_age2 = 0"), test = c("F"))

#The p value is less than 0.05 so we have statistical significance at the 5% level

#(k) This is just like j but we are now interested in the interaction terms
#between bachelor and age, which appear in reg7:

linearHypothesis(reg7, c("bac_age = 0 ", "bac_age2 = 0"), test = c("F"))

#The p value is greater than even 0.1, so we do not have statistical
#signifiance at the 10% level.

#(l)

#We have found that age has a significant effect on earnings, which is different
#for males and females and different for different age groups. We generally see
#a large coefficient on age, and a relatively small coefficient on age2,
#suggesting that age has a positive effect for younger individuals, which
#weakens over time and perhaps eventually becomes negative.

#We can show this more rigorously by estimating reg8 for different values of
#data, to see the effect of age for different groups of young people:

#We will predict the value of ln_ahe for each combination of female and bachelor,
#and for ages 25, 32 and 34.

#First define a data frame where we will store these;

#The matrix will have 4 rows for each of the 4 combinations of female and bachelor
#and 3 columns for each the 3 ages we are interested in.
dfln_ahe = data.frame(matrix(NA, nrow = 4, ncol = 3))
colnames(dfln_ahe) = c(25,32,34)
rownames(dfln_ahe) = c("male, HS", "male, Bachelor",
                       "female, HS", "female, Bachelor")

AGE = c(25,32,34)
FEMALE = 0
BACHELOR = 0

#This for loop will calculate a prediction for our given values
#of female and bachelor, as well as each of the ages we specified 
#in AGE. It will place the calcualtions into the dataframe we made, in the
#location appropriate for that age, ie age 25 will go in column i=1, 32 will
#go in column i =2, and age 34 will go in colum i =3. You don't need to know
#for loops for this tutorial, you could just calculate all of these estimates
#1 by 1, this is just for a little convenience. Alternatively, we could
#calculate our entire table with one for loop, but this would require a little
#bit more effort and familiarity with for loops. Instead here we will just
#make separate for loops for each case.

for (i in 1:3){
  dfln_ahe[1, i] = predict(reg8, newdata = data.frame(
    age = AGE[i],
    age2 = AGE[i]^2,
    fem_age = FEMALE * AGE[i],
    fem_age2 = FEMALE * AGE[i]^2,
    bac_age = BACHELOR * AGE[i],
    bac_age2 = BACHELOR * AGE[i]^2,
    female = FEMALE,
    bachelor = BACHELOR,
    fem_bac = FEMALE * BACHELOR
  ))
}


AGE = c(25,32,34)
FEMALE = 0
BACHELOR = 1

for (i in 1:3){
  dfln_ahe[2, i] = predict(reg8, newdata = data.frame(
    age = AGE[i],
    age2 = AGE[i]^2,
    fem_age = FEMALE * AGE[i],
    fem_age2 = FEMALE * AGE[i]^2,
    bac_age = BACHELOR * AGE[i],
    bac_age2 = BACHELOR * AGE[i]^2,
    female = FEMALE,
    bachelor = BACHELOR,
    fem_bac = FEMALE * BACHELOR
  ))
}


AGE = c(25,32,34)
FEMALE = 1
BACHELOR = 0

for (i in 1:3){
  dfln_ahe[3, i] = predict(reg8, newdata = data.frame(
    age = AGE[i],
    age2 = AGE[i]^2,
    fem_age = FEMALE * AGE[i],
    fem_age2 = FEMALE * AGE[i]^2,
    bac_age = BACHELOR * AGE[i],
    bac_age2 = BACHELOR * AGE[i]^2,
    female = FEMALE,
    bachelor = BACHELOR,
    fem_bac = FEMALE * BACHELOR
  ))
}

AGE = c(25,32,34)
FEMALE = 1
BACHELOR = 1

for (i in 1:3){
  dfln_ahe[4, i] = predict(reg8, newdata = data.frame(
    age = AGE[i],
    age2 = AGE[i]^2,
    fem_age = FEMALE * AGE[i],
    fem_age2 = FEMALE * AGE[i]^2,
    bac_age = BACHELOR * AGE[i],
    bac_age2 = BACHELOR * AGE[i]^2,
    female = FEMALE,
    bachelor = BACHELOR,
    fem_bac = FEMALE * BACHELOR
  ))
}

#We can also use this data to calculate the average percentage change
#in earnings per year, going from 25 to 32 as opposed to 32 to 34.
#You could calculate this using a for loop but you don't really need to know
#about those, and it is easy enough to do by hand:

#The difference between the predictions for ages 25 and 32, other things equal,
#is the percentage difference in earnings predicted between 25 and 32 year olds.
#Dividing this by the age difference, 32-25 = 7 gives the percentage change per 
#year.

#For the first row and 2 columns, we get the percentage difference in earnings
#per year from age 25 to 32:
(dfln_ahe[1,2] - dfln_ahe[1,1])/(32-25) * 100

#For the the first row and columns 2 and 3, we get the percentage difference in
#earnings per year from age 32 to 34:
(dfln_ahe[1,3] - dfln_ahe[1,2])/(34-32) * 100

#You can repeat this for each row:

#Males with Bachelor Degrees
(dfln_ahe[2,2] - dfln_ahe[2,1])/(32-25) * 100
(dfln_ahe[2,3] - dfln_ahe[2,2])/(34-32) * 100

#Females with High-School Degrees
(dfln_ahe[3,2] - dfln_ahe[3,1])/(32-25) * 100
(dfln_ahe[3,3] - dfln_ahe[3,2])/(34-32) * 100

#Females with Bachelor Degrees
(dfln_ahe[4,2] - dfln_ahe[4,1])/(32-25) * 100
(dfln_ahe[4,3] - dfln_ahe[4,2])/(34-32) * 100

#You may be able to see how a for loop could have made this easier (but you
#don't have to know about them so don't stress if you can't)

#Question 2:
#(a)
#Discuss the internal validity of the regressions that you used to answer
#Question 1. 

#See the tutorial 6 codeshare for the regressions calculated last time,
#and see the tutorial solutions posted to blackboard for a detailed
#explanation of the possible threats to internal validity.

#Before checking those answers you should consider whether any of the following
#are possibly the case herer:

#Omitted variable bias
#Mispecification of the functional form
#Errors-in-variables bias
#Sample selection bias
#Simultaneous Causality
#Inconsistency of OLS standard errors

#(b) To check external validity, we will run the same regressions on the 1992
#data and see if we get different results:

CPS = read_csv("cps92_12.csv") %>%
  mutate(cpi = 140.3*(year == 1992) + 229.6*(year == 2012),
         ahe12 = (ahe/cpi)*229.6) %>%
  mutate(ln_ahe12 = log(ahe12),
         ln_age = log(age),
         age2 = age*age,
         fem_bac = female*bachelor,
         fem_age = female* age,
         fem_age2 = female*age2,
         bac_age = bachelor* age,
         bac_age2 = bachelor* age2) %>%
  filter(year ==1992)

attach(CPS)

#Then running the same regressions again and outputting a table:

reg1 = lm_robust(ahe ~ age + female + bachelor, data =CPS,
                 se_type = "stata")
reg2 = lm_robust(ln_ahe12 ~ age + female + bachelor, data =CPS,
                 se_type = "stata")
reg3 = lm_robust(ln_ahe12 ~ ln_age + female + bachelor, data =CPS,
                 se_type = "stata")
reg4 = lm_robust(ln_ahe12 ~ age + age2 + female + bachelor, data =CPS,
                 se_type = "stata")
reg5 = lm_robust(ln_ahe12 ~ age + age2 + female + bachelor + fem_bac, data =CPS,
                 se_type = "stata")
reg6 = lm_robust(ln_ahe12 ~ age + age2 + fem_age + fem_age2 + female + bachelor +
                   fem_bac, data = CPS,
                 se_type = "stata")
reg7 = lm_robust(ln_ahe12 ~ age + age2 + bac_age + bac_age2 + female +
                   bachelor + fem_bac, data =CPS,
                 se_type = "stata")
reg8 = lm_robust(ln_ahe12 ~ age + age2 + fem_age + fem_age2
                 + bac_age + bac_age2 + female + bachelor + fem_bac,
                 data = CPS, se_type = "stata")

texreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8),
       include.cv = F, caption.above = T, digits = 3,
       caption = "Earnings and Age 2012",
       custom.model.names = c("(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)"))

#In fact, we end up with fairly similar results to our previous regressions, 
#suggesting there is decent external temporal validity.


#Question 3:

#(a)

#Copying over our regressions from the previous tutorial, along with some
#new, non- linear ones:

rm(list = ls())

BW<-read_csv("birthweight_smoking.csv") %>%
  mutate(young=as.numeric(age <=20),
         m_ed1 = as.numeric(educ < 12),
         m_ed2 = as.numeric(educ ==12),
         m_ed3 = (educ>12)*(educ<16),
         m_ed4 = as.numeric(educ == 16),
         m_ed5 = as.numeric(educ>16),
         age2 = age*age,
         smoker_age = smoker*age,
         smoker_young = smoker * young)
attach(BW)

reg1=lm_robust(birthweight ~smoker + alcohol+nprevist + unmarried, 
               data=BW,se_type ="stata")
reg2= lm_robust(birthweight ~smoker + alcohol+nprevist + unmarried+ 
                age +educ, data=BW, se_type ="stata")
reg3= lm_robust(birthweight ~smoker + alcohol+ nprevist+ unmarried
                + age + m_ed2+ m_ed3+ m_ed4+ m_ed5, data=BW,
                se_type="stata")
reg4= lm_robust(birthweight ~ smoker+ alcohol+ nprevist+ unmarried
                + m_ed2+ m_ed3+ m_ed4+ m_ed5+ young, data=BW,
                se_type="stata")
reg5= lm_robust(birthweight ~ smoker+ alcohol+ nprevist
                + unmarried + age + age2,data=BW, se_type="stata")
reg6= lm_robust(birthweight ~ smoker+ alcohol+ nprevist+ unmarried +
                  age + smoker_age,data =BW,se_type="stata")
reg7= lm_robust(birthweight ~ smoker+ alcohol+ nprevist
                + unmarried +young + smoker_young,data=BW, se_type="stata")

texreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7), include.ci = F, 
       caption.above = T, digits = 2, caption = "Birth Weight and Smoking",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"))

#In these regressions, we see several significant coefficients. The new 
#regressions show that there are new, significant interaction terms for the
#smoker variable, smoker_age and smoker_young. The other new regressions
#don't seem to find any new effects with the variables they add. Regressions 6
#and 7 suggest that the effect of smoking on BW depends on age, with the
#effect being higher as age increases. This would give a different
#estimate and thus different 95% confidence interval based on age.