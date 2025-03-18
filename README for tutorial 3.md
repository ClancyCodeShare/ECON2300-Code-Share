In the tutorial I was going to show you all how to use the command t.test() to check our confidence interval estimations, but I skipped over this as I was blanking on an explanation of how
to use this function and we were low on time.

If you look at the documentation, by typing ?t.test() into RStudio, you can see the first two variables are X and an optional variable Y.

t.test(X) will test the null hypothesis that the variable X is equal to 0 (at 5% significance level by default)

t.test(X,Y) will test the null hypothesis that the difference between variable X and variable Y is equal to 0.

Variable's X and Y will be a list of observed values. To check our answer for the confidence interval of the difference between mean birthweight for smokers and non-smokers, we could have written:

t.test(birthweight[smoker == 1], birthweight[smoker == 0])

By default, this will conduct a t test at 5% significance on the null hypothesis, H0: there is no difference in birthweight between smokers and non-smokers

We can see from the output (if you make sure to attach the data, attach(BW) ) that this gives us the same confidence interval that we hand calcualted in the tutorial.

The code share for this week also has an explanation of this command.

Thanks if you came to the tutorial and/or if you are reading this.
