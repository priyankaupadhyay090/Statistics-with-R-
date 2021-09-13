###############
### Cleaning Data - Exercise 1
###############

# Please do the "Cleaning Data with R" exercise that was assigned in DataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercises below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a DataCamp tutorial on how to further work with this data.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

# 1. Download the data file "digsym.csv" from the moodle and save it in your working directory.


# 2. Read in the data into a variable called "dat".
dat = read.csv("digsym-2.csv")


# 3. Load the libraries languageR, stringr, dplyr and tidyr.
library(languageR)
library(stringr)
library(dplyr)
library(tidyr)


# 4. How many rows, how many columns does that data have?
nrow(dat)
ncol(dat)
#It has 11 columns and 3700 rows.


# 5. Take a look at the structure of the data frame using "glimpse".
glimpse(dat)


# 6. View the first 20 rows, view the last 20 rows.
head(dat, 20)
tail(dat, 20)


# 7. Is there any missing data in any of the columns?
sum(is.na(dat))
colnames(dat)[colSums(is.na(dat)) > 0]
#Yes, there are 370 missing data points in column StimulDS1.RT.


# 8. Get rid of the row number column.
dat=dat[2:11]


# 9. Put the Sub_Age column second.
dat = dat[c(1,10,2,3,4,5,6,7,8,9)]



# 10. Replace the values of the "ExperimentName" column with something shorter, more legible.
dat = mutate(dat, ExperimentName = "DSK")


# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to dat and finally remove data2.
data2 = subset(dat, List != "Trial:1")
dat = data2
rm(data2)


# 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate".
dat = separate(dat, Sub_Age, c("Subject", "Age"), sep = " _ ")


# 13. Make subject a factor.
dat = mutate(dat, Subject=factor(dat$Subject))


# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".
dat = mutate(dat, File_new = str_remove_all(File, "[_1234567890]"))
        

# 15. Using str_pad to make values in the File column 8 chars long, by putting 0 at the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc).
dat = mutate(dat, File = str_pad(File, 8, side = "right", pad = "0"))




# 16. Remove the column "List".
dat$List <- NULL



# 17. Change the data type of "Age" to integer.
dat = mutate(dat, Age = as.integer(dat$Age))




# 18. Missing values, outliers:
# Do we have any NAs in the data, and if so, how many and where are they?
colnames(dat)[colSums(is.na(dat)) > 0]
#Not anymore. All 370 were in StimulDS1.RT.
#However, since we have deleted some rows in another task, there is no missing data anymore.



# 19. Create an "accuracy" column using ifelse-statement.
# If actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0.
dat = mutate(dat, accuracy = ifelse(StimulDS1.RESP == StimulDS1.CRESP, 1, 0 ))





# 20. How many wrong answers do we have in total?
count(filter(dat, accuracy== 0))
#answer: 185



# 21. What's the percentage of wrong responses?
count(filter(dat, accuracy== 0))/nrow(dat)
#answer 5.555556 %


# 22. Create a subset "correctResponses" that only contains those data points where subjects 
# responded correctly. 
correctResponses = filter(dat, accuracy== 1)




# 23. Create a boxplot of StimulDS1.RT - any outliers?
boxplot(dat$StimulDS1.RT)
#Yes, there are many outliers.


# 24. Create a histogram of StimulDS1.RT with bins set to 50.
hist(dat$StimulDS1.RT, breaks = 50)




# 25. Describe the two plots - any tails? any suspiciously large values?
# Both plots are positively skewed and there is one very large value (larger then 10000).



# 26. View summary of correct_RT.
#We assume that correct_RT is correctResponses.
summary(correctResponses)



# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named 
# "cleaned".
summary(dat)
cleaned = subset(dat, StimulDS1.RT < 13852)
summary(cleaned)














###############
### Exercise 2: Deriving sampling distributions
###############
## In this exercise, we're going to derive sampling distributions with 
## different sizes.

## a) Load the package languageR. We're going to work with the dataset 'dative'. 
## Look at the help and summary for this dataset.
library(languageR) #it is already loaded but okay
help(dative)
summary(dative)




## The term dative alternation is used to refer to the alternation between 
## a prepositional indirect-object construction
## (The girl gave milk (NP) to the cat (PP)) and a double-object construction 
## (The girl gave the cat (NP) milk (NP)).
## The variable 'LenghtOfTheme' codes the number of words comprising the theme.

## b) Create a contingency table of 'LenghtOfTheme' using table(). 
##    What does this table show you?

table(dative$LengthOfTheme)
#This table shows how many observations have a certain value in the column LenghtOfTheme. 
#E.g. there are 720 oberservation with value 1 for LengthOfTheme.








## c) Look at the distribution of 'LenghtOfTheme' by plotting a histogram and a boxplot. 
##    Do there appear to be outliers? Is the data skewed?
hist(dative$LengthOfTheme)
boxplot(dative$LengthOfTheme)
#Yes, there are outliers and the data is positively skewed.







## d) Now we're going to derive sampling distributions of means for different 
##    sample sizes. 
##    What's the difference between a distribution and a sampling distribution?

#The distribution is the distribution of the whole population, while a
#sampling distribution is the distribution of a sample drawn from a the population.





## e) We are going to need a random sample of the variable 'LengthOfTheme'. 
##    First create a random sample of 5 numbers using sample(). 
##    Assign the outcome to 'randomsampleoflengths'

randomsampleoflengths = sample(dative$LengthOfTheme, 5)





## f) Do this again, but assign the outcome to 'randomsampleoflengths2'. 

randomsampleoflengths2 = sample(dative$LengthOfTheme, 5)






## g) Now calculate the mean of both vectors, and combine these means 
##    into another vector called 'means5'.

means5 = c(mean(randomsampleoflengths), mean(randomsampleoflengths2))





## h) In order to draw a distribution of such a sample, we want means of 
##    1000 samples. However, we don't want to repeat question e and f 
##    1000 times. We can do this in an easier way: 
##    by using a for-loop. See dataCamp or the course books for 
##    how to write loops in R.

means5 = c()
for (val in seq(1, 1000)) {
  means5 = c(means5, mean(sample(dative$LengthOfTheme, 5)))
}





## i) Repeat the for-loop in question h, but use a sample size of 50. 
##    Assign this to 'means50' instead of 'means5'.

means50 = c()
for (val in seq(1, 1000)) {
  means50 = c(means50, mean(sample(dative$LengthOfTheme, 50)))
}






## j) Explain in your own words what 'means5' and 'means50' now contain. 
##    How do they differ?

#Means5 contains the means of 1000 different samples randomly drawn from the population with 5 data points each.
#Means50 contains the means of 1000 different samples randomly drawn from the population with 50 data points each.






## k) Look at the histograms for means5 and means50. Set the number of breaks to 15.
##    Does means5 have a positive or negative skew?

hist(means5, breaks=15)
hist(means50, breaks=15)
#Means5 is positively skewed.






## l) What causes this skew? In other words, why does means5 have bigger 
##    maximum numbers than means50?

#This is because means5 contains the means of smaller samples. And with smaller samples the mean is usually 
#further away from the mean of the population.









###############
### Exercise 3: Confidence interval
###############

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.

## a) What does a confidence interval mean from the perspective of experiment replication?


#It means that we are 95% certain, that if we replicate the experiment with a different sample, the mean of 
#this sample will be within the confidence interval.






## b) Let's calculate the confidence interval for our means from the previous 
##    exercise.
##    First, install and load the packages 'lsr' and 'sciplot'

library(lsr)
library(sciplot)




## c) Look at the description of the function ciMean to see which arguments it takes.

help(ciMean)





## d) Use ciMean to calculate the confidence interval of the dataset dative from
##    the previous exercise.
##    Also calculate the mean for the variable LengthOfTheme.

ciMean(dative)
mean(dative$LengthOfTheme)






## e) Does the mean of the sample fall within the obtained interval? 
##    What does this mean?

#Yes, it does and as far as we have understood how confidence intervals work, anything else would not make
#any sense, since we have used the same sample for both calculations. So the mean should lie exactly in
# the middle of the interval borders. Which it does.




## f) As the description of dative mentions, the dataset describes the 
##    realization of the dative as NP or PP in two corpora.
##    The dative case is a grammatical case used in some languages 
##    (like German) to indicate the noun to which something is given.
##    This dataset shows us, among other things, how often the theme is 
##    animate (AnimacyOfTheme) and how long the theme is (LengthOfTheme).
##    Plot this using the function bargraph.CI(). Look at the help for this function. 
##    Use the arguments 'x.factor' and 'response'.

help(bargraph.CI)
bargraph.CI(x.factor = dative$AnimacyOfTheme, response = dative$LengthOfTheme)





## g) Expand the plot from question f with the ci.fun argument 
##    (this argument takes 'ciMean'). 
##    Why does the ci differ in this new plot compared to the previous plot?

bargraph.CI(x.factor = dative$AnimacyOfTheme, response = dative$LengthOfTheme, ci.fun = ciMean)

#Because a different functions have been used to compute it. 






