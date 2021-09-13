### Stats with R Exercise sheet 7

##########################
#Week8: Checking Assumptions underlying ANOVA and linear regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 11. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete.

## Please write below your (and your teammates) name, matriculation number. 


## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)


###############################################################################
###############################################################################

########
### Exercise 1
########

########
### Please, use ggplot to make plots in all exercises below!
########


library(ggplot2)

setwd('D:\\Courses\\StatisticsWithR\\Assignments\\7')
getwd()

# a) Read in the data kidiq.txt (available in the Moodle) and take a look
#    at the data summary. It contains information about the mum's iq and 
#    their child's iq. 
#    mom_hs indicates whether the mother has a high school degree
#    1 = high school education, 0 = no high school degree.

data= read.table("kidiq.txt")
str(data)
data$mom_hs <- factor(data$mom_hs)
str(data)
head(data)


# b) Plot kid_score against mom_iq in a scatter plot, and add a regression line 
#    (kid_score should be the response variable and mom_iq the predictor 
#    variable) to the plot. 
#    Name the plot and the axis in sensible ways.


regression1 <- lm(kid_score~mom_iq, data=data)

graph1 <- ggplot(data, aes(mom_iq, kid_score)) +
  geom_point(color = "red") +
  geom_abline(intercept = coefficients(regression1)[1], slope = coefficients(regression1)[2], color="green") +
  ggtitle("SIMPLE REGRESSION (MOTHER IQ Vs KID SCORE)") +
  labs( x="MOTHER IQ ", y = "KID SCORE")
graph1

#OR
## graph1 = ggplot(data, aes(x = mom_iq, y = kid_score)) + geom_point() + geom_smooth(method=lm, formula = y~x)
## graph1



# c) State the main difference between correlation and regression .Calculate a simple regression model 
#for kid_score with mom_hs as a predictor and interpret the results.

## Both correlation and regression is used to find the relationship between the independent and dependent variables.
## Regression: mostly focuses on the cause of a variable with respect to the other. It finds the slope of the two variable.
## Correlation: gives the direction and the strength of the relationship between the variables. It doesn't give the change of a variable with respect to other.

summary(regression1)
## Observation: 
### RSE: 18.27 on 432 dof
### Multiple R-squared:  0.201,	
### Adjusted R-squared:  0.1991 
### F-statistic: 108.6 on 1 and 432 dof,  
### p-value: < 2.2e-16(0.00000000000000022, close to zero) which is significant 


# d) Next, fit a regression model with two predictors: mom_hs and mom_iq. 
#    Interpret the model 
#    Then compare this regression model to the previous model and state which has a better model fit.

regression2 <- lm(kid_score~mom_hs+mom_iq, data = data)
summary(regression2)

## Observation:
### Residual standard error: 18.14 on 431 dof
### Multiple R-squared:  0.2141 (Low)	
### Adjusted R-squared:  0.2105 
### F-statistic: 58.72 on 2 and 431 dof,  
### p-value: < 2.2e-16

## Comapring the both results (c) and (d), In this model we can clearly see the intercept which is the predicted kids score when
# both input is zero the r-squared value is low that is only
#  21.41% of the variance of the outcome is explained by the model which doesn't
# reflect good relationship of a model


# e) Now plot a model where both predictors are shown. Do this by plotting 
#    data points for mothers with high school degree==1 in one color and those 
#    without degree in another color. Then also fit two separate regression lines 
#    such that these lines reflect the model results.
#	   HINT: One solution in ggplot is to calculate fitted values of the regression model 
#    and then plot them along with the original data points:
#    pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, 
#    kid_score_pred=fitted(your_model))


pred1 <- data.frame(mom_iq=data$mom_iq, mom_hs=data$mom_hs, kid_score_pred=fitted(regression2))
graph2 <- ggplot(pred1, aes(mom_iq, kid_score_pred, color = mom_hs)) +
  geom_line() +
  geom_point(y = data$kid_score, x = data$mom_iq) +
  ggtitle("MULTIPLE REGRESSION (MOTHER IQ Vs KID SCORE)") +
  labs( x="MOTHER IQ", y = "KID SCORE")
graph2


# f) Next, we will proceed to a model including an interaction between mom_hs
#    and mom_iq. Fit the model and interpret your results.

## when we fit the model including an interaction, so it inclused both the predictors and the
## interaction b/w them. which is done by *
## so we introduce colinearity 

regression3 <- lm(kid_score~mom_hs*mom_iq, data = data)
summary(regression3)

# g) Next, let's plot the results of this model.

pred2 <- data.frame(mom_iq=data$mom_iq, mom_hs=data$mom_hs, kid_score_pred=fitted(regression3))
graph3 <- ggplot(pred2, aes(mom_iq, kid_score_pred, color = mom_hs)) +
  geom_line() +
  geom_point(y = data$kid_score, x = data$mom_iq) +
  ggtitle("MULTIPLE REGRESSION INCLUDING INTERACTION") +
  labs( x="MOTHER IQ", y = "KID SCORE")
graph3

# h) Next, let's explore the "predict.lm" function. Please first generate
#    a new dataframe with one datapoint (a mother with high school degree
#    and iq of 100). Then, use the predict function to predict the corresponding
#    child's iq. 
#    Please specify the predict function to also give you the 0.95 confidence 
#    interval.

test_new <- data.frame(mom_iq = c(100), mom_hs = c(1))
str(test_new)
summary(test_new)

test_new$mom_hs <- factor(test_new$mom_hs)
str(test_new)
summary(test_new)

predict(regression3, newdata = test_new, interval = "confidence")

# i) Meaning of confidence intervals for regression line.
#    Let's go back to exercise b) and plot again the data points with the 
#    regression line. By default, there should also be displayed the borders of 
#    the confidence interval. What is the meaning of this confidence interval?

graph1

## We can clearly see the borders of the confidence interval in this plot.
## Confidence interval gives us the actual(true) probability where the sample outcome is proportional to that of the population outcome.



# j) Finally, do model checking on your model from f), i.e. inspect 
#    the standard model plots provided by R, and interpret what you see.

plot(regression3)

## this model has almost best fit for our data, the regresion line is covering
## most of our data. 
