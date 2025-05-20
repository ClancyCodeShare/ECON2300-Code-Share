library(readr)
library(dplyr)
library(estimatr)
library(texreg)
library(car)

rm(list = ls())
smoking = read_csv("Smoking.csv") %>% mutate(age2 = age^2) 
attach(smoking)

#Different to previous
#smoking related data

getwd()
setwd("[Your working directory here]")

#Question 1:
#(a) Estimate the probability of smoking for all workers, and workers with
#and without smoking bans:
#For this, we are not conditioning on any data, we can just run a constant
#regression, which will output a constant value for any input. We don't need
#robust standard errors for this, as there are no indepenedent variables,
#so the depenedent variable cannot have different variances for different
#levels of the independents. So:

#All workers:

Pa = lm(smoker ~ 1, data = smoking)
P0 = lm(smoker ~ 1, data = subset(smoking, smkban == 1))
P1 = lm(smoker ~ 1, data = subset(smoking, smkban == 0))
#Could equivalently set data = smoking[smkban ==1,] to set smoking ban equal to 1

#We could also just take the averages for these groups:

mean(smoker)
mean(smoker[smkban==1])
mean(smoker[smkban==0])

#(b) What is the difference in the probability of smoking between workers
#affected by a workplace smoking ban and workers not affected? (Using linear
#probability model)

#For this we can just run a regular linear regression (although we should
#use heteroskedasticity robust errors this time, as this is no longer
#a constant regression):

lpm1 = lm_robust(smoker ~ smkban, data = smoking, se_type = "stata")

#B_1, in this case is the estimated difference in probability between
#the smoking ban and no smoking ban groups, and here it is about 0.08.

#We could also take the difference between means:
mean(smoker[smkban==1]) - mean(smoker[smkban==0])

#(c) Now estimate the same model a several more regressors:

lpm2 = lm_robust(smoker ~ smkban + female + age + age2 + hsdrop + hsgrad +
                   colsome + colgrad + black + hispanic,
                 data = smoking, se_type = "stata")

#Now compare the two regressions:

screenreg(list(lpm1, lpm2), include.ci = FALSE, 
          custom.gof.rows = list("SER" = c(sqrt(lpm1$res_var), sqrt(lpm2$res_var))))

#The more complex regression has slightly lower SER and decently higher adjusted
#R-sq, so we prefer it to the simple model. Also see that many of the new terms
#are statistically significant and the smkban coefficient is smaller, indicating
#we have probably controlled for some amount of meaningful OVB.

#(d) We are only testing one variable, so observe the t statistic, which is -5.
#so we have statistical significance at the 5% level (and decently beyond this)

#(e) Now we are doing a joint variable test, so we must conduct an F test,
#which tests the joint significance of a group of variables. One way of thinking
#of the F test is that it compares the variance of the model with the terms you
#are testing, to the model without them. If the model with the terms you are testing
#has substantially lower variance, then you can determine they have a jointly significant
#relationship with the dependent variable. 

linearHypothesis(lpm2, c("hsdrop =0", "hsgrad = 0", "colsome = 0", "colgrad = 0"),
                 test = c("F"))
#The test shows a high level of significance.
#So we can reject the null hypothesis.

#The estimated effect of colgrad, colsome and hsgrad, compared to having studied
#a masters degree, is a 0.04, 0.24 and 0.32 increase, corresponding to being
#4, 24 and 32 percentage points more likely to smoke than a masters degree holder.
#(Note the tight confidence intervals around these estimates, indicating we
#are pretty sure the true value is quite close to these estimates, although there
#is still always some margin of error)

#(f), (g) Repeat (c)-(e) using a probit model and then a logistic model

#The idea of this model is to construct a regression for the Z score of a
#dependent variable, Y, for a given independent variable(s) X.

#In rstudio we can estimate this using the glm function:

probit = glm(smoker ~ smkban + female + age + age2 + hsdrop + hsgrad +
               colsome + colgrad + black + hispanic, data = smoking,
             family = binomial(link = "logit"))
#This is all looks about the same as using lm or lm_robust, except at the
#end we have specified using "family" that we want a logit model.

#Logit models are almost the exact same, they just use a different
#probability function for estimating the probability of the dependnent variable

logit = glm(smoker ~ smkban + female + age + age2 + hsdrop + hsgrad +
              colsome + colgrad + black + hispanic, data = smoking,
            family = binomial(link = "probit"))
          
screenreg(list(logit, probit), include.ci = FALSE)

#We can see that smkban is still statistically signficant and the F tests show
#the same:

linearHypothesis(logit, c("hsdrop =0", "hsgrad = 0", "colsome = 0", "colgrad = 0"),
                 test = c("F"))
linearHypothesis(probit, c("hsdrop =0", "hsgrad = 0", "colsome = 0", "colgrad = 0"),
                 test = c("F"))

#It is difficult to compare the results with (b) because the estimated
#effect of smkban on smoking depends on the other values in the regression
#(since we are dealing with a regression that is no longer just a sum of 
#terms) (think about what must happen as the probability is close to 0 or 1,
#the effect of increasing the independent variables cannot be the same)(another
#insightful way to see this is to remember that the regression we are estimating
#is for a kind of Z score, not the probability itself, so you cannot just get the
#probability by subbing things into the regression we have calculated. We can
#however, still use the predict function).

#(h)
#We are going to make a whole bunch of predictions, we can use for loops
#to make this easier. We will be predicting for 2 different people accross all
#3 models. We can make this compact by writing a for loop that repeats the 2
#predictions for each model:

#First, write a list of the models we want to predict for:

models = list(lpm2, probit, logit)

#Then make a place to store the predictions:

predictions = data.frame(matrix(NA, ncol = 4, nrow = 3))
colnames(predictions) = c("Mr A, no ban","Mr A, ban", "Ms B, no ban", "Ms B, ban")
rownames(predictions) = c("Linear", "Probit", "Logit")
#This has just made a matrix full of NA's for us to store our predictions in.
#I have set the names so that we know where each prediction is from.

for (i in 1:3){
  A = predict(models[[i]], type = "response", 
              newdata = data.frame(smkban= c(0,1), age=20, age2=20^2,
                                   hsdrop=1, hsgrad=0, colsome=0,
                                   colgrad=0, female=0, black=0, hispanic=0))
  B = predict(models[[i]], type = "response", 
              newdata = data.frame(smkban= c(0,1), age=40, age2=40^2,
                                   hsdrop=0, hsgrad=0, colsome=0,
                                   colgrad=1, female=1, black=1, hispanic=0))
  predictions[i,c(1,2)] = A
  predictions[i,c(3,4)] = B
}
#Note the double square brackets on the models which accesses the terms from
#within the list, rather than reporting them as a list type object

#We can see that all of the models are pretty close together. If any is the odd
#one out, it is the linear model.

#Note that the difference the smoke ban makes is the same for A and B
#according to the linear model, as the linear model assumes that other
#attributes don't change the impact of the smoke ban (implausible assumption)