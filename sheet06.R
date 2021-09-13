### Stats with R Exercise sheet 6

##########################
# ANOVA
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 04. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates) name, matriculation number. 



## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################



#######################
### Exercise 1: Preparation
#######################

library(boot)
library(ggplot2)
library(reshape)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, 
# Cambridgeshire County Council considered 14 pairs of locations. The locations were 
# paired to account for factors such as traffic, volume and type of road. One site in 
# each pair had a sign erected warning of the dangers of speeding and asking drivers 
# to slow down. No action was taken at the second site. Three sets of measurements 
# were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the 
# erection of the sign, shortly after the erection of the sign, and again after 
# the sign had been in place for some time.

# a) For the further reference please use ?amis. 
# It may take some time to understand the dataset. 
?amis

# b) Load the dataset, store it into a variable called "data", and briefly inspect it. 
# Feel free to make some plots and calculate some statistics in order to understand 
# the data.
data = amis
head(data)
summary(data)


# c) All our columns have numeric type. Convert the categorial columns to factors.
cols <- c("period", "warning", "pair")
data[cols] <- lapply(data[cols], factor)


# d) Plot boxplots for the distribution of `speed` for each of the `period` values 
# (before, immediately after and after some time). Build 2 plots (each containing 3 
# boxplots) side by side depending on the `warning` variable.
# (For all plots here and below please use ggplot)
ggplot(data, aes(x=period, y=speed, fill=warning)) + 
  geom_boxplot() +
  facet_wrap(~warning)




# e) What can you conclude looking at the plots? What can you say about people's 
# behaviour in different periods: before, immediately after and after some time?

#We observe that after the errection of a warining sign the speed decreases a bit but only for some 
#drivers, other still drive with the same speed. However, after some time the speed increases even 
#more than before the errection of the sign.


# f) What are your ideas about why the data with warning==2 (sites where no sign was 
# erected) was collected?

#They were probably collected to check whether there are maybe other reasons for the change in speed
#not related to the warning (e.g. season change, increase in speeding ticket fares etc.).



#######################
### Exercise 2: 1-way ANOVA
#######################

# a) First let's create a new data frame which will be used for all exercise 2.
# For the 1-way ANOVA we will be working with a subset of `amis` using only the 
# data for sites where warning signs were erected, which corresponds to warning==1. 
# Therefore first subset your data to filter out warning==2 and then apply cast() 
# to average "speed" over each "pair" and "period". 
# Assign this new data frame to the variable casted_data.

casted_data = subset(data, warning==1)
casted_data = cast(casted_data, pair + period ~ ., fun.aggregate = mean , value = "speed", na.rm = T)
colnames(casted_data) = c("pair", "period", "speed")




# b) Build boxplots of the average speed depending on "period".

ggplot(casted_data, aes(x=period, y=speed)) + 
  geom_boxplot()





# c) Looking at the boxplots, is there a difference between the periods?

#Yes, people drive slower directly after the erection of the sign but faster than at before, some time after.





# Now, let's check the ANOVA assumptions and whether they are violated or not 
# and why.






# d) Independence assumption
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)

#There is not way to calculate this. However, we know that all observations 
#in our dataset are from different drivers. So even though we have three observations per
#pair, they are independent of each other, because they are from different drivers.







# e) Normality of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)

# fit a linear model
model = lm(speed ~ period, data = casted_data)
# Create a QQ plot of residuals
plot(model, 2)

#The QQ plot shows that all the points fall approximately along the reference line, we can assume normality.






# f) Homogeneity of variance of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)

plot(model, 1)

#The plot shows that there is no evident relationships between residuals and fitted values (the mean of each periods). 
#So, we can assume the homogeneity of variances.






# g) Now we are ready to perform 1-way ANOVA: please use the function aov() on the 
# speed depending on the period, report p-value and interpret the result in details.

summary(aov(casted_data$speed ~ casted_data$period))

#The one-way ANOVA (F(2,39)=0.986, p=0.382) showed no statistically significant 
#differences between period means.   






# h) what were the degrees of freedom from the result in part g)

#The degrees of freedom are 2 and 39.





# i) Calcuate the effect size and interpret the results. 
library(lsr)
etaSquared(aov(casted_data$speed~casted_data$period), anova=TRUE)

#We get an effect size of 0.0481, which we can interpret as a very small effect.



# j) Please do pairwise t-tests of the same variables as in g) using pairwise.t.test().

pairwise.t.test(casted_data$speed, casted_data$period)




# k) Report the pairwise p-values and interpret the result in detail.

#There are no statistically significant t-test results, which suggests 
#that there is no difference between any of the groups.





# l) Try to use no adjustment for pairwise testing and then the Bonferroni correction.
# Does the result change? 

pairwise.t.test(casted_data$speed, casted_data$period, p.adjust.method = "none")
pairwise.t.test(casted_data$speed, casted_data$period, p.adjust.method = "bonferroni")

#Yes, they changed, but there is still no statistically significant difference.




# m) If the results change why do they? What does Bonferroni correction do?


#The results were higher, because the non-adjusted results were multiplied by three.





# n) If the assumption of Normality does not hold, which test would you be using in this scenario.


#Mood’s median test or Kruskal-Wallis test.




#######################
### Exercise 3: 2-way ANOVA
#######################
# a) Now we want to analyze the influence of 2 categorial variables 
# (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1).
# First, we need to average the speed over each `pair`, `warning` and `period
# Cast your data again and assign the resuts to casted_data2.

casted_data2 = cast(data, pair + warning + period ~ ., fun.aggregate = mean , value = "speed", na.rm = T)
colnames(casted_data2) = c("pair", "warning", "period", "speed")







# b) Calculate the mean for each of the 6 possible pairs of `period` and `warning`.

mean(casted_data2$speed[casted_data2$warning==1 & casted_data2$period==1])
mean(casted_data2$speed[casted_data2$warning==1 & casted_data2$period==2])
mean(casted_data2$speed[casted_data2$warning==1 & casted_data2$period==3])

mean(casted_data2$speed[casted_data2$warning==2 & casted_data2$period==1])
mean(casted_data2$speed[casted_data2$warning==2 & casted_data2$period==2])
mean(casted_data2$speed[casted_data2$warning==2 & casted_data2$period==3])





# c) Do you think there is a significant difference between some of the groups?

#Yes, there is a big difference between the first group (warning = 1 and period = 1)
#and the last group (warning = 2 and period = 3).





# d) State the main difference between the applicability of 1-way and 2-way ANOVA.

#One-way ANOVA is applied when we have one independent variable (predictor) and two-way ANOVA when
#we have more than two predictors, the effects of which are not completely independent of one another.





# e) Now apply the 2-way ANOVA: please use the function aov() on the speed depending 
# on the period and warning.
# Report the p-value and interpret the result in detail. Properly formulate the findings!

summary(aov(speed ~ period + warning, casted_data2))

#We find a significant main effect of warning (F(1,80) = 8.533 ,p = .001), but 
#there is no significant main effect of period (F(2,80) = 1.127,p= 0.329). 





# f) What do you conclude about the behaviour of drivers based on the 2-way ANOVA?

#That the erection of warning signs is associated with a significant difference in the 
#drivers' speed.
