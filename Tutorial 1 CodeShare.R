#First off, don't worry that this looks like a lot of lines of code, most of
#this file is just text where I'm explaining what the code does. You can
#write text like this in your own code using the hashtag (#) symbol. Rstudio
#will ignore text after a #. This is helpful for writing notes on your code
#without breaking it.

#There's a few steps to do before we get started on solving the tutorial 
#problems

#First, clear your "Environment." 

#This is where RStudio keeps track of the data
#you are using. If you haven't used RStudio before, your environment should
#already be empty, but this is a good practice when writing a new code file,
#it keeps things tidy and ensures you aren't accidentally using unrelated data
#from a previous project

#To clear it, click the broom icon in the ribbon underneath the "Environment"
#tab.

#Alternatively, execute the following code:

rm(list = ls())

#rm() removes things from you environment, ls() lists everything in your
#environment


#Next we need to tell RStudio where our data is. Navigate to this folder
#and either take note of, or copy, its file path. Input this file path
#in the brackets of the following command.

setwd()

#For example:

setwd("C:/Users/username/Documents/ECON2300/Tutorial 1")

#Note that you need to write the file path inside of quotation ("") marks
#You can check if you have set the correct file path using:

getwd()

#In general, this is how you will start each of your code files.
#Now we can deal with the tutorial problems.

#(1)The text file consumption.txt contains observations on the weekly family 
#consumption expenditure (CONS) and income (INC) for a sample of 10 families.

#(a)Read the data into R.

#We are reading a txt files, these can be read with "read.delim"

read.delim("consumption.txt")

#Writing this outputs the data in the file into your console. Notice the table
#of data looks a little weird, the columns aren't centered, there is a different
#number of spaces separating some of the columns.

#To fix this, write:

read.delim("consumption.txt", header = TRUE, sep = "")

#Specifying that [header = TRUE] formats the first row as a header rather than
#part of the data. Setting [sep = ""] tells r that the columns are separated
#one or more spaces, so the columns can be alligned correctly.

#To get this data in your environment so we can use it later, assign it a
#variable:

mydata = read.delim("consumption.txt", header = TRUE, sep = "")

#now whenever you write [mydata], R will know to access the data you specified
#in the read.delim command. Try it:

mydata

#This outputs the same thing as the previous read.delim command.

#(b) Draw a scatter diagram of CONS against INC.

#To draw scatter diagrams we can use the plot() command.

plot(x = mydata$INC, y = mydata$CONS)

#You specify your x and y coordinates with sets of data. mydata$INC specifies
#the INC column in mydata, and mydata$CONS specifies the CONS column in mydata.

#This plot is a little bare though, instead we could write:

plot(x = mydata$INC, y = mydata$CONS, 
     main = "Consumption Data", 
     xlab = "Income", ylab = "Consumption",
     pch = 19)

#main sets the title, xlab and ylab set the x and y axis labels, and pch sets
#the point style.


#(c) On checking the data, you find that your assistant has recorded the weekly
#consumption expenditure for Family 8 as $900 instead of $90. Correct this error 
#and redraw the scatter diagram.

#This question requires us to use indexing. We can do this using square brackets

#eg:

mydata[3,1]

#this outputs the value in the third row and first column of mydata.
#We are interested in the cell for consumption for family 8. Looking at our
#data table, we can see this is in row 8, column 1. We can check this:

mydata[8,1]

#Now we want to correct this value. To do so, simple set it equal to the correct
#value, 90, using an equals sign

mydata[8,1] = 90

#We can check if this worked:

mydata

#Observe that the value has changed from 900 to 90.

#(d) RStudio has commands to handle each of these calculations:

mean(INC)
median(INC)
max(INC)
min(INC)

mean(CONS)
median(CONS)
max(CONS)
min(CONS)

#However, we can save some time by using the [summary()] command, which
#outputs several relevant statistics, including the ones asked for in the question

summary(INC)

summary(CONS)


#Compute the correlation coefficient between CONS and INC. Comment on the result.

#RStudio has a command for calculating correlation: cor()

cor(mydata)

#Using the command on your data will output a correlation matrix, where the
#off diagonals are the correlation values between INC and CONS

#Alternatively, you can take the correlation between INC and CONS by inputting
#each into the command:

cor(mydata$CONS, mydata$INC)



#(f)Create the following new variables:
#DCONS = 0.5CONS
#LCONS = log(CONS)
#INC2 = INC^2
#SQRTINC = INC^(1/2)

#We can do this by specifying new variables with these names, and applying
#these operations using various functions provided by RStudio:

DCONS = 0.5*CONS
#Multiplication uses the * symbol

LCONS = log(CONS)
#log() takes a logarithm, by default using base e.

INC2 = INC^(2)
#Exponents are written using the ^ symbol. The brackets are unnecessary here,
#but for a more complicated exponent like say, 2+x, the brackets are important
#to keep the terms together.

SQRTINC = sqrt(INC)
#sqrt() takes the square root of the term inside the brackets

#(g) Delete the variables DCONS and SQRTINC

#For this we can use the rm() command, or equivalently, remove()

remove(DCONS, SQRTINC)

#Check that these variables are no longer in your environment

#(h) Delete everything

#For this, we use the same command we used to clear out environment in the
#beginning:

rm(list = ls())


#2 At the Famous Fulton Fish Market in New York city, sales of whiting 
#(a type of fish) vary from day to day. Over a period of several months, daily 
#quantities sold (in pounds) were observed. These data are in the file 
#fultonfish.dat. Description of the data is in the file
#fultonfish.def. Describe the first four columns.

#(a) Use R to open the data file and name the series in the first four columns
#as date, lprice, quan and lquan


#This time the data is in a .dat file. This can also be read using read.delim.
#As before, we can output the data directly into our console using simply:

read.delim("fultonfish.dat")

#First you'll notice this is a bigger file.

#Also, there is no header, the columns are unlabeled. Fortunately, the columns
#are alligned better than in the previous file (You may see this better by
#opening the file in a text editor, as the R console display doesn't fit
#the data in as neatly).

#We can put this data in our environment by giving it a name and adjusting
#some inputs in read.delim():

fultonfish = read.delim("fultonfish.dat", header = FALSE, sep = "")

#We can take a better look at our data now by using View()

View(fultonfish)

#Everything looks good, except, as we noted before, there's no names on the
#columns. Fortunately, R has a well named function for editing column names:
#colnames()

colnames(fultonfish)
#This outputs the current column names

colnames(fultonfish)[1]
#This gives teh first column name, using an index with square brackets, set to 1
#We can also index to multiple indices:

colnames(fultonfish)[1:4]
#1:4 out puts the integers from 1 to 4, so this gives the first 4 column names

#We want to rename the first 4 columns as instructed in the question. To do so:

colnames(fultonfish)[1:4] = c("data", "lprice", "quan", "lquan")
View(fultonfish)
#using c() creates a vector of the terms we input inside the brackets,
#this allows us to assign column names in order, the first column name is 
#set to the first object in our vector, and so on.

#(b) Compute the sample mean and standard deviation of the quantity sold (quan).

#Unfortunately, the summary command from before does not output standard
#deviation, so we will just compute this each individually.

mean(fultonfish$quan)
sd(fultonfish$quan)
#sd() computes the sample standard deviation, as indicated by its documentation
#which tells us it uses n-1 as the denominator. To access this, execute 
#help(sd()) or ?sd()


#(c) Test the null hypothesis that the mean quantity sold is equal to 7,200 
#pounds a day at the 5% level of significance.

#Using the above sample mean and standard deviation, we can perform a hypothesis
#test so long as we know the appropriate t score.

#You could access a t chart, or r can tell you the t score using qt()
#By checking help(qt()), you can see this does a one tail test, either on the
#top or bottom tail, based on the lower.tail input.

#Hence the p value will be 0.025, ie half the value given for our 0.05 level
#of significance. Degrees of freedom are n-1 = 110.

qt(0.025, df = 110, lower.tail = FALSE)
#setting lower.tail is false just gives us a positive rather than negative value

#Now you can use the formula for a 1 sample hypothesis test, getting you t value
#by taking the difference between your sample and hypothesised mean, and
#dividing by standard error.

#RStudio also has a handy command to do this all for us:

t.test(fultonfish$quan, mu = 7200)

#the first input is our data, and the second, mu, is the value for the mean
#we are testing in our hypothesis test.

#The t score is higher than the value with calculated, and the p value is less
#than 0.05. Each of these tells us that the mean is statistically significantly
#different to 7200 at the 5% significance level.

#(d)

#This step is probably easier to do on pen and paper, but the calculations can
#also be done inside of RStudio.

#The upper and lower bounds of the confidence interval can be calculated by
#taking the sample mean, plus/minus the standard error times our t critical value

#Since the sample size is large, n = 111 > 30, we can approximate the t critical
#value at the 5% significance level to be 1.96, ie the critical value at 5%
#significance in the standard normal distribution.

CI = c(mean(fultonfish$quan)-1.96*(sd(fultonfish$quan)/sqrt(length(fultonfish$quan))), 
       mean(fultonfish$quan)+1.96*(sd(fultonfish$quan))/sqrt(length(fultonfish$quan)))
CI

#Alternatively, we could use the t.test() function again and specify a 95% 
#confidence level. This will use the precise t score rather than our
#approximation to the normal distribution.

t.test(fultonfish$quan, mu = 7200, conf.level = 0.95)
#The interval is slightly larger, since approximating the t distribution to
#the standard normal distribution yields a smaller score value. (Though not
#much smaller when n is large)

#(e)
#Plot lprice against lquan and label the variable lprice as “log(Price) of 
#whiting per pound” and lquan as “log(Quantity)”. 
#Then, comment on the nature of the relationship between these two variables.

#We can use the plot() command again here, inputting sensible values for the
#axis labels and title, and setting a style with pch = 19.

plot(fultonfish$lquan, fultonfish$lprice, 
     main = "Log Price and Log Quantity",
     xlab = "log(Quantity), in pounds", 
     ylab = "log(Price) of whiting per pound", 
     pch = 19)

#Just by visually analysing the data, it looks like there is a weak negative
#relationship between log(quantity) and log(price).

#We can check this by calculating the correlation coeficient:
cor(fultonfish$lquan, fultonfish$lprice)

#We see a relatively small negative value, supporting the weak negative 
#correlation that we observed on the plot.

#(f) Save this workfile to any folder on any drive.

#You can do this by using the save buttong around the top left of RStudio,
#or you can use a command:

save(list = ls(all= TRUE), file = "tut01.RData")
#This will save it in your current working directory.