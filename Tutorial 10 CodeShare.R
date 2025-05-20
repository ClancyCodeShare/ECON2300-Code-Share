install.packages("AER")

library(readr)    # package for fast read rectangular data
library(dplyr)    # package for data manipulation
library(estimatr) # package for commonly used estimators with robust SE
library(texreg)   # package for converting R regression output to LaTeX/HTML tables
library(car)      # package for functions used in "An R Companion to Applied Regression"
library(multcomp) # package for simultaneous tests and CIs for general linear hypotheses
library(AER)      # package for functions used in "Applied Econometrics using R"

# Clean Working Environment
rm(list = ls())

#Set working directory

#getwd()
#setwd("[your working directory here]")

#Alternatively:
# To use the following line: 
# save this file in the same directory as the data files
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

Fertility = read_csv("Fertility.csv")
attach(Fertility)

#Question 1

#(a)
OLS = lm_robust(weeksm1 ~ morekids, data = Fertility, se_type = "stata")
summary(OLS)
#morekids appears to have a statistically significant, negative relationship
#with weeksm1

#(b)

#The regression in (a) is inappropriate for predicting causal effects,
#because it likely suffers from simultaneous causality bias. Having
#more kids may lead you to work less, however, working more may also
#make you less likely to have more kids. This will make morekids correlate
#with the error term in the regression, so we will have a biased estimate
#of the effect of morekids on weeksm1.

#(c)

OLS.first1 = lm_robust(morekids ~ samesex, data = Fertility, se_type = "stata")
summary(OLS.first1)
#samesex appears to have a notable and statistically significant positive
#relationship with morekids.

#(d)

#On its face, samesex has a plausible relationship with morekids - 
#parents may wish to have at least one child of each sex and so
#may have another if the first two are the same - but they might
#have stopped at 2 if they were the different sexes.

#Additionally, there is no clear reason why samesex would have
#any relationship with weeksm1 - except for the indirect
#effect through morekids. So samesex should be uncorrelated with
#the error term in the first linear regression we ran. 

#So we have decent reason to suspect that samesex is endogenous and relevant,
#making it a plausible instrumental variable for our first regression.

#We could check further:

cor(samesex,morekids)
linearHypothesis(OLS.first1, "samesex = 0", test = "F")

#(e) samesex is not a weak instrument, it has a very strongly significant
#correlation with morekids. Although the correlation itself is not particularly
#large, we have strong statistical evidence that it is distinct from 0,
#and that is what matters in this determination.

#(f) 

TSLS1 = ivreg(weeksm1 ~ morekids | samesex)
summary(TSLS1)
#alternatively, we could make the two-stage regression using
#the tools we already have:

#First step, predict the value of more kids using our previous regression
#for each value of samesex in the dataset
FS_morekids <- predict(OLS.first1, newdata = data.frame(samesex = samesex))

#Then, run a regregression of weeksm1 onto these predictions of morekids
TSLS1_byhand <- lm_robust(weeksm1 ~ FS_morekids, se_type = "stata")
summary(TSLS1_byhand)

#(g)

TSLS2 = ivreg(weeksm1 ~ morekids + agem1 + black + hispan + othrace |
                samesex + agem1 + black + hispan + othrace)
screenreg(list(TSLS1, TSLS2))

#Only a small difference, not especially meaningful. We can see
#there likely was not any OMV in (f), because samesex is not 
#related to any of the new variables (and (f) was effectively a
#regression on samesex, since it was the instrument for predicting
#morekids).


#Or we could do this more manually:

OLS.first2 = lm_robust(morekids ~ samesex + agem1 + black + hispan + othrace,
                       se_type = "stata")
FS_morekids2 = predict(OLS.first2, newdata = data.frame(samesex = samesex, 
                                                         agem1 =  agem1,
                                                         black = black, 
                                                         hispan = hispan,
                                                         othrace = othrace))
TSLS2_byhand = lm_robust(weeksm1 ~ FS_morekids2 + agem1 + black + hispan + 
                            othrace, se_type = "stata")
summary(TSLS2_byhand)


# comparing the impact of adding new regressors with just morekids.
TSLS2_byhand2 = lm_robust(weeksm1 ~ FS_morekids + agem1 + black + hispan + 
                             othrace, se_type = "stata")
summary(TSLS2_byhand2)

#Question 2
detach(Fertility)
rm(list = ls())

Movies = read_csv("Movies.csv") %>%
  mutate(ln_assaults = log(assaults),
         attend = attend_v + attend_m + attend_n)
attach(Movies)

#(a)i

reg1 = lm_robust(ln_assaults ~ year2 + year3 + year4 + year5 + year6 + year7 + 
                   year8 + year9 + year10 + month2 + month3 + month4 + month5 + 
                   month6 + month7 + month8 + month9 + month10 + month11 + 
                   month12, se_type = "stata")
summary(reg1)

#The null-hypothesis is non-seasonality, ie all of the months have the same
#predicted level of assaults, so the difference between the included and dummies
#the excluded dummy should be 0, that is, every month dummy variable should equal
#0. So test this hypothesis with an F-test
linearHypothesis(reg1, c("month2=0", "month3=0", "month4=0", "month5=0",
                         "month6=0", "month7=0", "month8=0", "month9=0",
                         "month10=0", "month11=0", "month12=0"), test=c("F"))
#We find there is strong evidence for seasonality.

#ii
reg2 = lm_robust(attend ~ year2 + year3 + year4 + year5 + year6 + year7 + 
          year8 + year9 + year10 + month2 + month3 + month4 + month5 + 
          month6 + month7 + month8 + month9 + month10 + month11 + 
          month12, se_type = "stata")
summary(reg2)
linearHypothesis(reg2, c("month2=0", "month3=0", "month4=0", "month5=0",
                         "month6=0", "month7=0", "month8=0", "month9=0",
                         "month10=0", "month11=0", "month12=0"), test=c("F"))
#Again, strong evidence of seasonality.

OLS = lm_robust(ln_assaults ~ attend_v + attend_m + attend_n + year2 + year3 + 
                  year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
                  month2 + month3 + month4 + month5 + month6 + month7 + month8 + 
                  month9 + month10 + month11 + month12 + h_chris + h_newyr + 
                  h_easter + h_july4 + h_mem + h_labor + w_maxa + w_maxb + 
                  w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow,
                se_type = "stata")
summary(OLS)

#Attendence of moderately violent and violent movies
#predicts an approximate 0.32% increase in assaults per
#million attendees, statistically
#significant at the 1% level.

#ii

linearHypothesis(OLS, c("attend_v = attend_m"), test=c("F"))

linearHypothesis(OLS, c("attend_v = attend_n"), test=c("F"))

linearHypothesis(OLS, c("attend_v=attend_m", "attend_v=attend_n"), test=c("F"))

#Do not have strong evidence to suggest that the relationship
#is different between strongly, moderately and non-violent movies

#iii
confint(glht(OLS, linfct = c("6*attend_v - 2*attend_m - attend_n = 0")))

#Confidence interval for the effect of the given change in attendence
# [-0.0204, -0.0008] with an estimate of -0.0106

#Alternatively, check the hint in the tutorial and you can
#make a regression like this:

X1 = attend_v/6
X2 = attend_v/3 + attend_m
X3 = attend_v/6 + attend_n

reg = lm_robust(ln_assaults ~ X1 + X2 + X3 + year2 + year3 + 
                  year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
                  month2 + month3 + month4 + month5 + month6 + month7 + month8 + 
                  month9 + month10 + month11 + month12 + h_chris + h_newyr + 
                  h_easter + h_july4 + h_mem + h_labor + w_maxa + w_maxb + 
                  w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow, se_type = "stata")
summary(reg)

#We end up with the same confidence interval and estimate this way.

#(c)
TSLS1 = ivreg(ln_assaults ~ attend_v + attend_m + attend_n +
                year2 + year3 + year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
                month2 + month3 + month4 + month5 + month6 + 
                month7 + month8 + month9 + month10 + month11 + month12 +
                h_chris + h_newyr + h_easter + h_july4 + h_mem + h_labor +
                w_maxa + w_maxb + w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow |
                pr_attend_v + pr_attend_m + pr_attend_n +
                year2 + year3 + year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
                month2 + month3 + month4 + month5 + month6 + month7 +
                month8 + month9 + month10 + month11 + month12 +
                h_chris + h_newyr + h_easter + h_july4 + h_mem + h_labor +
                w_maxa + w_maxb + w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow)

summary(TSLS1)

#A statistically significant reduction in assaults is predicted
#by an increase in violent movie attendance.

# (ii) 
linearHypothesis(TSLS1, c("attend_v=attend_m"), test=c("F"))

linearHypothesis(TSLS1, c("attend_v=attend_n"), test=c("F"))

linearHypothesis(TSLS1, c("attend_v=attend_m", "attend_v=attend_n"), test=c("F"))

# The F-statistic suggests that the coefficients Beta_v, Beta_m, and Beta_n are
# not statistically significantly different from one another.

# (iii) 
confint(glht(TSLS1, linfct = c("6*attend_v - 2*attend_m - attend_n = 0")))

# The TSLS estimate for this coefficient is -0.013. It shows a decrease in
# assaults of 1.3%. The 95% confidence interval is -0.024 to -0.0016 (or -2.4%
# to -0.16%).

#(We could also use the same approach as before of manipulating the
#regression to get this confidence interval)

## (d)
# (i) 
TSLS2 = ivreg(ln_assaults ~ attend_v + attend_m + attend_n +
                year2 + year3 + year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
                month2 + month3 + month4 + month5 + month6 + month7 +
                month8 + month9 + month10 + month11 + month12 +
                h_chris + h_newyr + h_easter + h_july4 + h_mem + h_labor +
                w_maxa + w_maxb + w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow |
                attend_v_f + attend_m_f + attend_n_f + attend_v_b + attend_m_b + attend_n_b +
                year2 + year3 + year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
                month2 + month3 + month4 + month5 + month6 + month7 +
                month8 + month9 + month10 + month11 + month12 +
                h_chris + h_newyr + h_easter + h_july4 + h_mem + h_labor +
                w_maxa + w_maxb + w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow)

summary(TSLS2)

# The results are shown in the column labeled TSLS in Table 3. An increase in
# strongly violent movie attendance of one million viewers is predicted to
# reduce assaults by 0.32%. The coefficient is not statistically significant at
# the 10% significance level.

# (ii) 
linearHypothesis(TSLS2, c("attend_v=attend_m"))

linearHypothesis(TSLS2, c("attend_v=attend_n"))

linearHypothesis(TSLS2, c("attend_v=attend_m", "attend_v=attend_n"))

# The F-statistic suggests that the coefficients Beta_v, Beta_m, and Beta_n are
# not statistically significantly different from one another.

# (iii) 
confint(glht(TSLS2, linfct = c("6*attend_v - 2*attend_m - attend_n = 0")))

# The TSLS estimate for this coefficient is -0.008. It shows a decrease in
# assaults of 0.8%. The 95% confidence interval is -0.027 to 0.011 (or -2.7% to
# 1.1%).

# The texreg to print the table with the main regressors.
texreg(list(OLS, TSLS1, TSLS2), include.ci = F, caption.above = T, digits = 4,
       caption = "Violent Movie and Violent Behavior",
       custom.model.names = c("(1) OLS", "(2) IV", "(3) TSLS"),
       omit.coef = "(year)|(month)|(h_)|(w_)")

#(e)

TSLS3 = ivreg(ln_assaults ~ attend_v + attend_m + attend_n +
                year2 + year3 + year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
                month2 + month3 + month4 + month5 + month6 + month7 +
                month8 + month9 + month10 + month11 + month12 +
                h_chris + h_newyr + h_easter + h_july4 + h_mem + h_labor +
                w_maxa + w_maxb + w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow |
                pr_attend_v + pr_attend_m + pr_attend_n +
                attend_v_f + attend_m_f + attend_n_f + attend_v_b + attend_m_b + attend_n_b +
                year2 + year3 + year4 + year5 + year6 + year7 + year8 + year9 + year10 + 
                month2 + month3 + month4 + month5 + month6 + month7 +
                month8 + month9 + month10 + month11 + month12 +
                h_chris + h_newyr + h_easter + h_july4 + h_mem + h_labor +
                w_maxa + w_maxb + w_maxc+ w_mina + w_minb + w_minc + w_rain + w_snow)

summary(TSLS3, diagnostics = T)
#Observe the J statistic, with the row name "Sargan"
#The p value is non-significant, so we do not reject
#the null hypothesis that the excess instruments are exogenous

#(f)

#We have found statistically significant evidence that 
#watching violent movies is associated with lower levels
#of assaults. This could be because watching movies is a substitute
#for other behaviours that are more likely to get people into fights,
#eg going out drinking.