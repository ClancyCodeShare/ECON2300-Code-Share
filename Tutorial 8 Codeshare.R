library(readr)
library(dplyr)
library(estimatr)
library(texreg)

#New packages:
install.packages(c("plm", "fastDummies"))

library(plm)        #Package for estimating with linear panel data
library(fastDummies)#Package for creating dummy/indicator variables

#Clear your environment and set the working directory:

rm(list = ls())
getwd()
setwd("[your working directory here]")

#Get the data into your environment, mutating in some log variables

Guns = read_csv("Guns.csv") %>%
  mutate(lvio = log(vio), lrob = log(rob), lmur = log(mur))
attach(Guns)

#Question 1:

#(a) Regress lvio ~ shall, and lvio ~ shall + inc_rate + density + avginc + pop
#+ pb1064 + pw1064 + pm1029


pols1 = lm_robust(lvio ~ shall, data = Guns, se_type = "stata", clusters = stateid)

pols2 = lm_robust(lvio ~ shall + incarc_rate + density + avginc +
                    pop + pb1064 + pw1064 + pm1029, data = Guns, 
                  se_type = "stata", clusters = stateid)

#(i) Strongly significant negative estimate of shall.
#The estimate is highly meaningful, indicating that shall
#laws predict a roughly 36% decrease in the rate of violent incidents

#(ii) The control variables do not change the strong
#statistical significance, although they do reduce the magnitude of the estimate
#from 44% to 36%. This is a meaningful difference in estimates, although they are
#both still high estiamtes

#(iii) We can think of several omited effects here, such as attitudes towards guns,
#quality of police, big cities vs more rural.


#(b)

fe1 = plm(lvio ~ shall + incarc_rate + density + 
            avginc + pop + pb1064 + pw1064 + pm1029,
          data = Guns, model = "within", index = c("stateid", "year"))
#Setting model to "within" tells plm to run a fixed effects regression, with 
#indexes given by the index setting.

#Before analysing statistical significance, we need to correct our standard errors.
#By default the errors in plm() are not cluster robust, ie they are not robust to
#the possibility of serial correlation within the cluster groups. Eg high violence
#in a state one year correlates with high violence the next year.

#To fix our standard errors, do the following:
SE.fe1 = sqrt(diag(vcovHC(fe1, type = "sss", cluster = "group")))

#We set cluster = "group" when the model is only considering state based effects

#We can use the standard errors to calculate the p values:

p.fe1 = 2*(1-pnorm(abs(fe1$coefficients/SE.fe1)))

#pnorm the area under the standard normal distribution from negative infinity up
#to our normalised coefficient. Taking 1 minus this value gives the one tail
#p value and multiplying it by 2 gives the 2 tail p value.

#We see that using these standard errors, shall's coefficient is no longer
#statistically significant. Additionally, its estimate is a lot lower, only
#around -4% and without statistical significance, we cannot rule out an effect
#of 0%.

#Evidently, there was important ommitted variable bias in the previous regressions.
#(b)'s regression is more credible.

#(c)

fe2 = plm(lvio ~ shall + incarc_rate + density + avginc +
            pop + pb1064 + pw1064 + pm1029,
          data = Guns, model = "within", index = c("stateid", "year"),
          effect = "twoways")

#Or equivalently, just include time as a factor:

fe3 = plm(lvio ~ shall + incarc_rate + density + avginc +
            pop + pb1064 + pw1064 + pm1029 + factor(year),
          data = Guns, model = "within", index = c("stateid", "year"))

#Setting effect to "twoway" now allows for fixed state and time effects.
#By default, effect is set to "individual" which only allows for state based
#effects.

#Again, we need to adjust our standard errors to account for clustering:

SE.fe2 = sqrt(diag(vcovHC(fe2, type = "sss", cluster = "group")))

p.fe2 = 2*(1-pnorm(abs(fe2$coefficients/SE.fe1)))

#We see the estimate on shall has decreased (in magnitude) again and is still 
#insignificant.

#We can test if the time effects were statistically significant by testing
#the difference between fe1 and fe2:

pFtest(fe2, fe1)

#We see that the F test for the fixed effects is strongly significant,
#indicating that fe2 picked up on ommitted variable bias that existed in fe1.

#(d)

#Perform the same analysis on the lrob and lmur variables:

#We will copy over the code, changing the lvio variable accordingly and put the
#results into a table each time:

#lrob:

pols1 = lm_robust(lrob ~ shall, data = Guns, se_type = "stata", clusters = stateid)

pols2 = lm_robust(lrob ~ shall + incarc_rate + density + avginc +
                    pop + pb1064 + pw1064 + pm1029, data = Guns, 
                  se_type = "stata", clusters = stateid)

SE.pols1 <- pols1$std.error
SE.pols2 <- pols2$std.error
p.pols1 <- 2*(1 - pnorm(abs(pols1$coefficients/SE.pols1)))
p.pols2 <- 2*(1 - pnorm(abs(pols2$coefficients/SE.pols2)))

fe1 = plm(lrob ~ shall + incarc_rate + density + 
            avginc + pop + pb1064 + pw1064 + pm1029,
          data = Guns, model = "within", index = c("stateid", "year"))
SE.fe1 = sqrt(diag(vcovHC(fe1, type = "sss", cluster = "group")))
p.fe1 = 2*(1-pnorm(abs(fe1$coefficients/SE.fe1)))

fe2 = plm(lrob ~ shall + incarc_rate + density + avginc +
            pop + pb1064 + pw1064 + pm1029,
          data = Guns, model = "within", index = c("stateid", "year"),
          effect = "twoways")
SE.fe2 = sqrt(diag(vcovHC(fe2, type = "sss", cluster = "group")))
p.fe2 = 2*(1-pnorm(abs(fe2$coefficients/SE.fe1)))

texreg(list(pols1, pols2, fe1, fe2), include.ci = F, caption.above = T, digits = 3,
       override.se = list(SE.pols1,SE.pols2,SE.fe1,SE.fe2),
       override.pvalues = list(p.pols1, p.pols2, p.fe1, p.fe2),
       caption = "Robbery Rate and Shall-Carry Law",
       custom.model.names = c("(1) Pooled OLS (1)", "(2) Pooled OLS",
                              "(3) Fixed Effects", "(4) Fixed Effects & Time Effects"))

#lmur:

pols1 = lm_robust(lmur ~ shall, data = Guns, se_type = "stata", clusters = stateid)
pols2 = lm_robust(lmur ~ shall + incarc_rate + density + avginc +
                    pop + pb1064 + pw1064 + pm1029, data = Guns, 
                  se_type = "stata", clusters = stateid)
SE.pols1 <- pols1$std.error
SE.pols2 <- pols2$std.error
p.pols1 <- 2*(1 - pnorm(abs(pols1$coefficients/SE.pols1)))
p.pols2 <- 2*(1 - pnorm(abs(pols2$coefficients/SE.pols2)))

fe1 = plm(lmur ~ shall + incarc_rate + density + 
            avginc + pop + pb1064 + pw1064 + pm1029,
          data = Guns, model = "within", index = c("stateid", "year"))
SE.fe1 = sqrt(diag(vcovHC(fe1, type = "sss", cluster = "group")))
p.fe1 = 2*(1-pnorm(abs(fe1$coefficients/SE.fe1)))

fe2 = plm(lmur ~ shall + incarc_rate + density + avginc +
            pop + pb1064 + pw1064 + pm1029,
          data = Guns, model = "within", index = c("stateid", "year"),
          effect = "twoways")
SE.fe2 = sqrt(diag(vcovHC(fe2, type = "sss", cluster = "group")))
p.fe2 = 2*(1-pnorm(abs(fe2$coefficients/SE.fe1)))

texreg(list(pols1, pols2, fe1, fe2), include.ci = F, caption.above = T, digits = 3,
       override.se = list(SE.pols1,SE.pols2,SE.fe1,SE.fe2),
       override.pvalues = list(p.pols1, p.pols2, p.fe1, p.fe2),
       caption = "Murder Rate and Shall-Carry Law",
       custom.model.names = c("(1) Pooled OLS (1)", "(2) Pooled OLS",
                              "(3) Fixed Effects", "(4) Fixed Effects & Time Effects"))

#(e) There may be two-way causality between the crime dependent variables and
#incarceration rate. Higher level of crime would lead to high incrate, so the
#dependent variable would be affecting the independent. This could create
#a two-way causality problem

#(f) Our most credible estimate is given in the fixed state and time effects
#regression, which gives a small, non-significant estimate, so we cannot rule out
#the possibility that there is no effect from shall laws.
