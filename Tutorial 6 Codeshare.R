#Clear the environment and set your working directory:

rm(list = ls())
getwd()
setwd("[your working directory here]")

#Access the necessary packages:

library(readr) # package for fast read rectangular data
library(dplyr) # package for data manipulation
library(ggplot2) # package for elegant data visualisations
library(estimatr) # package for commonly used estimators with robust SE
library(texreg) # package converting R regression output to LaTeX/HTML tables
library(car) # package for functions used in "An R Companion to Applied Regression"
library(multcomp) # package for simultaneous tests and CIs for general linear hypotheses

#use install.packages("[package name]") for any packages you have not installed

#Question 1

#We will need to access the lead mortality data, so use read_csv to
#read the file into your environment:

LM = read_csv("lead_mortality.csv")
attach(LM)
#(a)

#Compute the average infant mortality rate, infrate, for cities with lead pipes
#and with non-lead pipes. Is there a statistically significant difference in the
#average?

#We can take the sample means simply with:

mean(infrate[lead == 0]) #0.3811679
mean(infrate[lead==1]) #0.4032576

#The difference is:
mean(infrate[lead==1])-mean(infrate[lead==0]) #0.02208973

#This is a substantial difference, although we do not know if it is significant
#yet. To test this, we need the standard error. We can access this from our
#regression which gives us robust standard errors.

reg1 = lm_robust(infrate ~ lead, data = LM, se_type = "stata")
summary(reg1)

#See that the slope coefficient is the same as the difference between
#means we calculated, as the slope coefficient represents the expected
#difference between the lead and non-lead groups. The standard error
#is large, even larger than the estimate, giving a t value of 0.9,
#and hence no statistical signficance at even the 10% significance level.

#(b)

# The amount of lead leached from lead pipes depends on the chemistry of the 
#water running through the pipes. The more acidic the water (that is, the lower 
#its pH), the more lead is leached.Run a regression of infrate on lead, ph, and 
#the interaction term lead × ph.

#To access this interaction term, we will need to update our data table
#with the mutate command from dplyr:

LM = read_csv("lead_mortality.csv") %>%
  mutate(lead_ph = lead*ph)
attach(LM)

reg2 = lm_robust(infrate ~ lead +ph + lead_ph, data = LM, se_type = "stata")
summary(reg2)

#(i)

#The intercept is the expected infant mortality rate with
#lead and ph equal to 0.The other three variables are somewhat interconnected
#lead + lead_ph gives the expected difference in infant mortality rate
#between the lead and non-lead groups. ph+lead_ph gives the expected effect
#of an extra unit of ph on infant mortality rate. See that the effect is just
#equal to ph if lead=0.

#(ii)

fig1 = ggplot(LM, aes(x=ph, y=infrate, col = as.factor(lead))) +
  labs(title = "Figure 1: Infant MOrtality Rate and pH Value",
       x= "PH Value", y = "Infant Mortality Rate") +
  geom_smooth(data = LM, method = "lm", se = FALSE, size = 1) +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, size = 12, family = "serif",
                                  face = "bold"),
        legend.position = c(0.9,0.8),
        legend.title = element_blank(),
        legend.text = element_text(family = "serif", face = "bold"),
        legend.key = element_rect(color = "transparent"),
        legend.background = element_rect(fill = "lightgrey",
                                         size = 0.8,
                                         linetype = "solid")) +
  scale_color_discrete(name = "Lead", labels = c(" lead = 0", " lead = 1"))
print(fig1)

#See from this plot that as ph increases, the expected difference in
#infant mortality rate between the lead and non lead groups decreases,
#until it flips after a pH of about 8. To quantify this relationship,
#consider 10th, 50th and 90th percetiles of pH:

quantile(LM$ph, probs = c(0.1,0.5,0.9))

#Then evaluate the predicted effect of lead at each of these quantiles

predict(reg2, newdata = data.frame(lead = 1, ph = 6.4, lead_ph = 6.4)) -
  predict(reg2, newdata = data.frame(lead = 0, ph = 6.4, lead_ph = 0))

predict(reg2, newdata = data.frame(lead = 1, ph = 7.5, lead_ph = 7.5)) -
  predict(reg2, newdata = data.frame(lead = 0, ph = 7.5, lead_ph = 0))

predict(reg2, newdata = data.frame(lead = 1, ph = 8.2, lead_ph = 8.2)) -
  predict(reg2, newdata = data.frame(lead = 0, ph = 8.2, lead_ph = 0))

#(iii)

# Does lead have a statistically significant effect on infant mortality? Explain

#We cannot just test the lead coefficient, as the effect of lead is also
#tied to lead_ph, with the total effect being the sum of their coefficients.
#Hence, we must joint significance test them, to see if at least one is significant.

linearHypothesis(reg2, c("lead = 0", "lead_ph = 0"), test = c("F"))

#We get a p value less than 0.05 so we have significance of at least
#one of the lead related variables at the 5% significance level.

#(iv)

#Does the effect of lead on infrate depend on ph? Is this dependence statistically
#significant?

#Whether the effect of lead depends on ph is tied up the the lead_ph variable's
#coeficient. One of the interpretations of this coeficient is the additional
#impact of lead on infant mortality rate for an additional unit of ph.

#We see that there is a substantial negative estimate, predicting
#a decline of 5 percentage points in the impact of the lead variables, for
#each additional unit of ph. In other words, higher ph appears to have some
#protective effect on the harms of lead (if we assume a causal relationship).

#This effect is signficant, as we can  see from the t value, p value and
#confidence interval

#(v)

mean(LM$ph) #7.322674

#The average effect of lead on infant mortality at this pH level is,
#based on coefficients 2 and 4 in reg2:

#0.46180 - 0.05687 * 7.322674 = 0.04535953

sd(LM$ph) #0.6917287

mean(LM$ph) - sd(LM$ph) #6.630946

#The average effect of lead on infant mortality at this pH level is,
#based on coefficients 2 and 4 in reg2:

#0.46180 - 0.05687 * 6.630946 = 0.0846981

#And if the pH was one standard deviation higher than the mean:

#0.46180 - 0.05687 * (7.322674 + 0.6917287) = 0.006020918

#(vi) 

#Construct a 95% confidence interval for the effect of lead on infant
#mortality when ph = 6.5

#We want a confidence interval for B_1 + 6.5B_3

#We can use confint() and glht to construct a confidence interval for a fitted model
#(such as reg2)

confint(glht(reg2, linfct = c("lead + 6.5*lead_ph=0")))

#glht specifies a hypothesis to test, inside of confint which gives a confidence
#interval for our hypothesis.

#Alternatively, we could rearrange our regression so that the first slope
#coefficient corresponds to the expected effect of lead on infrate, when 
#ph is 6.5, as follows:

# y = B_0 + (B_1 * lead) + (B_2 * ph) + (B_3 * lead * ph) + u

# y = B_0 + (B_1 * lead) + (B_2 * ph) + (B_3 * lead * ph) + 
#                                       (B_3 * 6.5lead - B_3 * 6.5lead) + u

# y = B_0 + (B_1 * lead + B_3 * 6.5lead) + (B_2 * ph) + 
#                                     (B_3 * lead * ph - B_3 * 6.5lead) + u

# y = B_0 + (B_1 + 6.5B_3) * lead + (B_2 * ph) + B_3*lead(ph - 6.5) + u

#Since B_1 + 6.5 * B_3 is just a constant, this can be estimted by
#just running a regression of infrate on lead, ph and (lead * (ph - 6.5))

#See that what was the lead * ph term on the right now is equal to 0 when ph
#is 6.5, so the effect of lead when ph is 6.5 is captured entirely by 
#B_1+6.5*B_3. We can hypothesis test this coefficient using outputs from a
#regression.

#To do this, we will use mutate again to add a column for values of:
#lead * (ph - 6.5)

#We will also add in our previous mutate column.

LM = read_csv("lead_mortality.csv") %>% 
  mutate(lead_ph = lead * ph, lead_ph_65 = lead * (ph-6.5))
attach(LM)

reg3 = lm_robust(infrate ~ lead + ph + lead_ph_65, data = LM,
                 se_type = "stata")
summary(reg3)

#With all of our regressions run, we can use texreg to create a latex
#output for a nice table of them all:

texreg(list(reg1, reg2, reg3), include.ci = F, caption.above = T,
       digits = 4, caption = "Lead and Infant Mortality",
       custom.modelnames = c("1", "2", "3"))
#We can use the outputs here to construst the same confidence interval
#as before, using our estimate of the slope coefficient on lead, +/-
#1.96 times its standard error:

#We can also access these directly from our regression:

CI_lead_65 = c(reg3$coefficients[2] - 1.96 * reg3$std.error[2],
               reg3$coefficients[2] + 1.96 * reg3$std.error[2])

#(c) Try running the same regression from part (b) but add in possible
#ommitted variables, and see if you find a substantial change in any of
#your other estimates.

#Question 2

#Here we use the cps12.csv dataset, so remove what we had before and add this
#one

rm(list = ls())

#We will mutate our data to add in additional variables for our regressions
CPS12 = read_csv("cps12.csv") %>%
  mutate(ln_ahe = log(ahe),
         ln_age = log(age),
         age2 = age*age,
         fem_bac = female*bachelor,
         fem_age = female*age,
         fem_age2 = female * age2,
         bac_age = bachelor * age,
         bac_age2 = bachelor * age2)
attach(CPS12)

#Then each of our regressions are simply (noting that reg5 onward apply to tut 7):
  
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
reg6 = lm_robust(ln_ahe ~ age + age2 + fem_age + fem_age2 + female + bachelor, data = CPS12,
                 se_type = "stata")
reg7 = lm_robust(ln_ahe ~ age + age2 + bac_age + bac_age2 + female +
                   bachelor + fem_bac, data =CPS12,
                 se_type = "stata")
reg8 = lm_robust(ln_ahe ~ age + age2 + fem_age + fem_age2
                 + bac_age + bac_age2 + female + bachelor + fem_bac,
                 data = CPS12, se_type = "stata")

texreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8),
       include.ci = F, caption.above = T,
       digits = 3, caption = "Earnings and Age, 2012",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"))

#Then for each regression we want to find the expected
#change in the dependent variable for a change in age from 25 to 26
#and from 34 to 35 (except (a) where it is 33 to 34)

#We can access the results from the table generated by texreg and then
#check them using the predict command:

#(a)

#from 25 to 26 and  from 33 to 34:
#The coefficient on age is 0.510, and this is a linear
#regression, so in both cases the expected increase in hourly
#wages is $0.510

predict(reg1, newdata = data.frame(
  age = 26,
  female = mean(female),
  bachelor = mean(bachelor)
)) -
  predict(reg1, newdata = data.frame(
    age = 25,
    female = mean(female),
    bachelor = mean(bachelor)
  ))

predict(reg1, newdata = data.frame(
  age = 34,
  female = mean(female),
  bachelor = mean(bachelor)
)) -
  predict(reg1, newdata = data.frame(
    age = 33,
    female = mean(female),
    bachelor = mean(bachelor)
  ))

#(b)

#We are now dealing with a logarithm on our dependent
#variable, so our slope coefficients represent a percentage
#change in the dependent variable, corresponding a unit
#change in the independent variables.

#Hence the coeficient of 0.026 on age means we are predicting
#a 2.6% increase in hourly wages when age increases by 1, this
#applies to the difference between age 25 and 26, and age 34 and 35

predict(reg2, newdata = data.frame(
  age = 26,
  female = mean(female),
  bachelor = mean(bachelor)
)) -
  predict(reg2, newdata = data.frame(
    age = 25,
    female = mean(female),
    bachelor = mean(bachelor)
  ))

predict(reg2, newdata = data.frame(
  age = 35,
  female = mean(female),
  bachelor = mean(bachelor)
)) -
  predict(reg2, newdata = data.frame(
    age = 34,
    female = mean(female),
    bachelor = mean(bachelor)
  ))

#(c) The same again for regression 3, this time we have ln_age so input log(26)
#when age is 26, and so on.
predict(reg3, newdata = data.frame(
  ln_age = log(26),
  female = mean(female),
  bachelor = mean(bachelor)
)) -
  predict(reg3, newdata = data.frame(
    ln_age = log(25),
    female = mean(female),
    bachelor = mean(bachelor)
  ))

predict(reg3, newdata = data.frame(
  ln_age = log(35),
  female = mean(female),
  bachelor = mean(bachelor)
)) -
  predict(reg3, newdata = data.frame(
    ln_age = log(34),
    female = mean(female),
    bachelor = mean(bachelor)
  ))

#(d) and the same again for reg4, inputting 26^2 for age2 when age is 26, and so
#on

predict(reg4, newdata = data.frame(
  age = 26,
  age2= 26^2,
  female = mean(female),
  bachelor = mean(bachelor)
)) -
  predict(reg4, newdata = data.frame(
    age = 25,
    age2 = 25^2,
    female = mean(female),
    bachelor = mean(bachelor)
  ))

predict(reg4, newdata = data.frame(
  age = 35,
  age2 = 35^2,
  female = mean(female),
  bachelor = mean(bachelor)
)) -
  predict(reg4, newdata = data.frame(
    age = 34,
    age2=34^2,
    female = mean(female),
    bachelor = mean(bachelor)
  ))

#(e)

#The differene between reg2 and reg3 is very small. Adding the extra variable
#does not increase R^2 much and decreases adjusted R^2, although only by a small
#amount. Hence we prefer reg2 although the difference is small.

#(f)

#reg4 adds the age^2 variable to reg2, and the coefficient on this variable is
#not significant. We prefer reg2 as a more simple model with similar qualities
#to reg4 otherwise. However the differene is so small that either model could be
#used to get nearly the same result.

#(g)

#These regressions differ in that they both add a term to reg2 which ends up not
#being statistically significant and having small effects on the overall model.
#By comparing on the basis of mulitple R^2, we see that reg4 has a higher score
#than reg3 so given they are otherwise quite similar, we slightly prefer reg4.



