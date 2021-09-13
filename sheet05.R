### Stats with R Exercise sheet 5

##########################
#Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, December 14. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.


## Please write below your (and your teammates') name, matriculation number. 
## Name: 
## Matriculation number: 

## Change the name of the file by adding your matriculation numbers

###########################################################################################
###########################################################################################


library(reshape)
library(languageR)
library(ggplot2)
library(ggpubr)

#######################
### Exercise 1: Correlation
#######################

# a) Get some data - access the ratings data set in languageR and name it "data".
# The data set contains subjective frequency ratings and their length averaged over 
# subjects, for 81 concrete English nouns.
data <- ratings

# b) Take a look at the data frame.
summary(data)

# c) Let's say you're interested in whether there is a linear relationship between 
# the word frequency of the 81 nouns and their length.
# Take a look at the relationship between the frequency and word length data by 
# means of a scatterplot (use the ggplot library for this).

ggplot(data, aes(Frequency, Length)) + geom_point()


# d) Judging from the graphs, do you think that word frequency and word length are 
# in any way correlated with one another?

# Answer: word frequency increses and word lenght is decresing - negative relationship


# e) Compute the Pearson correlation coefficient for the two variables by means 
# of cor().
# Tell R to only include complete pairs of observations.
# As a reminder: Pearson coefficient denotes the covariance of the two variables 
# divided by the product of their respective variance. 
# It is scaled between 1 (for a perfect positive correlation) to -1 (for a perfect 
# negative correlation).

cor(data$Frequency, data$Length, use = "complete.obs")


# f) Does the correlation coefficient suggest a small, medium or large effect?
# What about the direction of the effect?

#Answer: Medium negative coefficient ( -0.4281462)


# g) Note that we have a large number of tied ranks in word length data 
# (since there are multiple words with the length of e.g. 5).
# Thus, we might draw more accurate conclusions by setting the method to 
# Kendall's tau instead of the Pearson correlation coefficient (which is the default).
# How do you interpret the difference between these 2 correlation coefficients?

cor(data$Frequency, data$Length, use = "complete.obs", method = "kendall")

# Answer: There is a difference in correlation. 
# Pearson's correlation use  mean and standard deviation from the mean, 
# and Kendall's tau correlations use ordinal information of pairs.


# h) What about significance? Use the more user-friendly cor.test()!
# Take a look at the output and describe what's in there.
# What do you conclude?

cor.test(data$Frequency, data$Length)

#Answer: -0.4281462 = not significant 

# i) Finally, also calculate Spearman's rank correlation for the same data.

cor(data$Frequency, data$Length, use = "complete.obs", method = "spearman")



#######################
### Exercise 2: Regression
#######################

# a) Fit a linear regression model to the data frame "data" from exercise 1 
# for the variables Frequency (outcome variable) and Length (predictor variable).
# General form: 
# "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"

linear_regr <- lm(Frequency ~ Length, data = data)

# b) How do you interpret the output? Is the relationship between the two variables 
# positive or negative?
# Plot the data points and the regression line.

linear_regr
ggplot(data, aes(Frequency, Length))+ geom_point()+ geom_smooth(method='lm')

#Answer - negative linear relationship



# c) Run the plotting command again and have R display the actual words that belong 
# to each point. 
# (Don't worry about readability of overlapping words.)

ggplot(data, aes(Frequency, Length))+ geom_point()+ geom_smooth(method="lm")+
  geom_text(aes(label=Word),hjust=0, vjust=0)

#######################
### Exercise 3: Regression
#######################


# a) Try this again for another example:
# Let's go back to our digsym data set.
# Set your wd and load the data frame digsym_clean.csv and store it in a variable. 

setwd('D:\\Courses\\StatisticsWithR\\Assignments\\4')
getwd()
data1 <- read.csv('digsym_clean.csv')

# b) Suppose you want to predict reaction times in the digit symbol task by 
# people's age.
# Fit a linear regression model to the data frame for the variables 
# correct_RT_2.5sd (outcome variable) and Age (predictor variable).
# General form: 
# "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
  
# But first we need to cast the data to compute an RT mean (use correct_RT_2.5sd) 
# for each subject, so that we have only one Age observation per Subject.
# Store the result in a new dataframe called "cast".
# In case you're wondering why we still have to do this - like the t-test, 
# linear regression assumes independence of observations.
# In other words, one row should correspond to one subject or item only.


cast = cast(data1, Subject + Age ~., fun.aggregate = mean , value = "correct_RT_2.5sd", na.rm = T)

colnames(cast) = c("Subject", "Age", "correct_RT_2.5sd")


# c) Now fit the regression model.

linear_regr11 = lm(correct_RT_2.5sd ~ Age, data=cast)
  

# d) Let's go over the output - what's in there?
# How do you interpret the output?
linear_regr11

#Answer, if predictiors variable (Subject's age) = 0 then response time would be 637.93 years old
#And that an increase of one year in the Age, corresponds to a increase of 637.93 in response time.


# e) Plot the data points and the regression line.

plot_1 <- ggplot(cast, aes(correct_RT_2.5sd, Age))+ geom_point()+ geom_smooth(method='lm')

#Answer positive linear regression 


# f) Plot a histogram and qq-plot of the residuals. 
# Does their distribution look like a normal distribution?

hist(resid(linear_regr11))
qqnorm(resid(linear_regr11))

#The histogram suggests - normal distribution 
# qq-plot suggests - No normal distribution 




# g) Plot Cook's distance for the regression model from c) which estimates the 
# residuals (i.e. distance between the actual values and the  predicted value on 
# the regression line) for individual data points in the model.

plot(cooks.distance(linear_regr11))



# h) Judging from the plot in g) it actually looks like we have 1 influential 
# observation in there that has potential to distort (and pull up) our regression 
# line.
# The last observation (row 37) in cast has a very high Cook's distance 
# (greater than 0.6).
# In other words, the entire regression function would change by more than 
# 0.6 when this particular case would be deleted.
# What is the problem with observation 37?
# Run the plotting command again and have R display the subjects that belong to 
# each point.


data1 = data.frame(cast$Subject, cooks.distance(linear_regr11))

colnames(data1) = c("Subject", "cooks_distance")

ggplot(data1, aes(Subject, cooks_distance))+ geom_point()+
  geom_text(aes(label=Subject),hjust=0, vjust=0)

# i) Make a subset of "cast" by excluding the influential subject and name it cast2.

cast2 = subset(cast, Subject != "40")

# j) Fit the model from c) again, using cast2, and take a good look at the output.

linear_regr12 = lm(correct_RT_2.5sd ~ Age, data=cast2)

linear_regr12

# k) What's different about the output?
# How does that change your interpretation of whether age is predictive of RTs?

# Now an increase of 1 year in Age, suggests an increase of 862.05 in response time.



# l) Plot the regression line again - notice the difference in slope in 
# comparison to our earlier model fit?

plot_2= ggplot(cast2, aes(Age, correct_RT_2.5sd))+ geom_point()+
  geom_smooth(method="lm")

#Answer =higher intercept and a flatter slope.


# m) Display the two plots side by side to better see what's going on.

library(cowplot)

plot_grid(plot1, plot2)

# n) Compute the proportion of variance in RT that can be accounted for by Age.
# In other words: Compute R Squared.
# Take a look at the Navarro book (Chapter on regression) if you have trouble 
# doing this.

summary(linear_regr11) #R squared is 0.1779
summary(linear_regr12) #R squared is 0.03493

# o) How do you interpret R Squared?

#according to (R)^2 = first model have better response time

