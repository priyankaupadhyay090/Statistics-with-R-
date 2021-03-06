### Stats with R Exercise sheet 4

##########################
#Week 5: t-test and friends
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, December 7. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students but only one student needs to submit the solution!

## Please write below your (and your teammates') name, matriculation number. 


## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################

###############
### Cleaning Data
###############
install.packages("lsr")
install.packages("tidyr")
install.packages("effsize")
install.packages("doBy")

library(lsr)
library(tidyr)
library(effsize)


# 1. Download the data file "digsym_clean.csv" from the moodle and save it in your 
# working directory. 


# 2. Read in the data into a variable called "data".
data = read.csv("D:\\Courses\\StatisticsWithR\\Assignments\\4\\digsym_clean.csv")


# 3. Get rid of the column "X"
data = subset(data, select = -c(X))


# Say you're interested in whether people respond with different accuracy to 
# right vs. wrong picture-symbol combinations.
# In other words, you want to compare the average accuracy for the digsym-right 
# and digsym-wrong condition.
# Like the conscientious researcher you are, you want to take a look at the data 
# before you get into the stats.
# Therefore, you will need to create a barplot of the mean accuracy data 
# (split out by condition) using ggplot and the summarySE function (given below).
# Let's do it step by step.

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
  # data: an input dataframe
  # measurevar: a column name of <data> (as string), on which we would like to calculate 
  #             standard deviation (SD), standard error (SE) and confidence interval (CI).
  # groupvars: categorical columns of <data> (as vector of strings ) which we would like to use
  #            to make all possible combinations for which we calculate SD, SE, CI based 
  #            on <measurevar>.
  # na.rm: should we remove NA
  # conf.interval: confidence interval
  library(doBy)
  
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
    
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  # Calculate standard error of the mean
  datac$se <- datac$sd / sqrt(datac$N)  
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# 4. Apply the function summarySE on the accuracy data grouping by right/wrong condition
# (use the provided documentation inside the function above for the arguments description).

data_acc = summarySE(data, "accuracy", c("condition"))

# 5. Create the barplot (use ggplot2 for this and all tasks below) with error bars 
# (which the function summarySE readily provided).
# Gauging from the plot, does it look like there's a huge difference in accuracy 
# for responses to the right and wrong condition?
library(ggplot2)
ggplot(data_acc, aes(x=condition, y=accuracy, fill=condition))+ geom_bar(stat="identity")



# 6. Let's go back to our data frame "data", which is still loaded in your console
# Now that you've taken a look at the data, you want to get into the stats.
# You want to compute a t-test for the average accuracy data in the right and 
# wrong condition.
# Why can't you compute a t-test on the data as they are now? 
# Hint: Which assumption is violated?


# 7. We need to reshape the data to only one observation (average accuracy) per subject 
# and right/wrong condition. Here we will use cast() which we discussed in the tutorial
# for sheet 2. 
# Collapse the data, 
# using cast(data, var1 + var2 + var3 ... ~, function, value = var4, na.rm = T).
# Store the result in a new variable called "cdata". 
## Check ?cast or https://www.statmethods.net/management/reshape.html for more infos on 
## cast(). 





# 8. Create histograms of the accuracy data depending on the right and wrong 
# condition and display them side by side.


# 9. Display the same data in density plots. 


# 10. Based on the histograms and the density plots - are these data normally 
# distibuted?


# 11. Create boxplots of the accuracy data.


# 12. Compute the t-test to compare the mean accuracy between wrong and right picture
# combinations.
# Do you need a paired t-test or independent sample t-test? why?


# 13. What does the output tell you? What conclusions do you draw?


# 14. Compute the effect size using CohensD.


# 15. Which effect size do we get? How do you interpret this result?


# 16. In addition to the long-format data we've just been working on, you may also 
# encounter data sets in a wide format (this is the format we have been using in 
# class examples.)
# Let's do a transformation of our data set (cdata) to see what it would look like in a wide 
# format.
# Use spread() from the tidyr package.


# 17. Compute the t-test again on the wide format data - note that for wide-format 
# data you need to use a different annotation for the t-test.


# 18. Compare the t-test results from the wide-format and the long-format data. 
# What do you notice?


# 19. Compute CohensD on the wide format data. What do you notice?



# 20. Let's try the t-test again, but for a different question:
# Suppose you are interested in whether reaction times in the digit symbol 
# task differ depending on gender.
# In other words, you want to test whether or not men perform significantly 
# faster on average than women, or vice versa.
# Collapse the original data, using 
# cast(data, var1 + var2 + var3 ... ~ ., function, value = var4, na.rm = T).
# Store the result in a new variable called "cdat"


# 21. Take a look at cdat using head().


# 22. Compute the t-test to compare the accuracy means of female and male 
# participants.
# Which t-test do you need and why? How do you interpret the result?


###############
### T-Test
###############
#In this exercise we will try to explore the independent samples t-test 
#and its affect on different samples. 
#We will take the same example discussed in the lecture. A class has two tutors, and we want to find out which tutor is better by
#comparing the performance of the students in the final exam by tutor group. 

#1. Generate 10 samples from a normal distribution with mean 0 and sd 10 and save it a variable names "first_tutor_grades"

first_tutor_grades = rnorm(n = 10, mean = 0, sd = 10)


#2. Create a vector named "first_tutor" having same 10 values -> "tutor1"
first_tutor = rep(c("tutor1"),each=10)


#3. Create a data frame named "data_frame" having 2 columns "first_tutor", "first_tutor_grades" created above. 
data_frame = data.frame(first_tutor,first_tutor_grades)

#4. Change the column names of the data frame to "tutor" and "score"
colnames(data_frame) = c("tutor", "score")


#5. repeat the steps 1-4 with the following changes:
  #i) generate another 10 samples with mean 10 and sd 25. save it in a variable: second_tutor_grades
  #ii)Create a vector named "second_tutor" having 10 same values -> "tutor2"
  #iii) Create a data frame named "data_frame2" having 2 columns "second_tutor", "second_tutor_grades" created above.
  #iv) Change the column names of the data frame to "tutor" and "score"
 

second_tutor_grades = rnorm(n = 10, mean = 10, sd = 25)
second_tutor_grades
second_tutor <- rep(c("tutor2"),each=10)
data_frame2 <- data.frame(second_tutor, second_tutor_grades)
colnames(data_frame2) <- c("tutor", "score")


#6. combine both data frames into a new one and name it "final_df"
# final_df should have 2 columns (tutor, score) having 20 rows. e.g.
#   tutor      score
#1  tutor1     9.09
#2  tutor1     4.66
#3  tutor1     3.56
#4  tutor2     1.56
#5  tutor2     545

final_df <- rbind(data_frame, data_frame2)
final_df

#7. run the independent samples TTest (independentSamplesTTest) and formulate the findings as discussed in the lecture. 
#	What do you observe? 
#	independentSamplesTTest also provides the effect size (Cohen's d). How do you interpret the effect size?

final_df$tutor = factor(final_df$tutor)

independentSamplesTTest(formula = score ~ tutor, data = final_df, var.equal = TRUE)

# tutor1 (mean = -5.076,std dev. = 10.055,  tutor2 (mean = 8.401, std dev. = 22.488)
# significant (t(18)=-1.73, p < 0.101, CI = [-29.843, 2.888], d = 0.774)

#8. Time to play around!
#	repeat the whole experiment you performed above with different sample size, mean and standard deviation  
#	repeat it 3 times changing all the values (sample size, mean, sd) and formulate the findings.  
#	what do you observe when we keep the means and sd same?



# First repetition: when sample size, mean = same, sd= different 


first_tutor_grades = rnorm(n = 10, mean = 0, sd = 25)
first_tutor = rep(c("tutor1"),each=10)
data_frame = data.frame(first_tutor,first_tutor_grades)
colnames(data_frame) = c("tutor", "score")



second_tutor_grades = rnorm(n = 10, mean = 10, sd = 5)
#second_tutor_grades
second_tutor <- rep(c("tutor2"),each= 10)
data_frame2 <- data.frame(second_tutor, second_tutor_grades)
colnames(data_frame2) <- c("tutor", "score")



final_df= rbind(data_frame, data_frame2)
final_df
final_df$tutor = factor(final_df$tutor)
independentSamplesTTest(formula = score ~ tutor, data = final_df, var.equal = TRUE)


#              tutor1 tutor2
# mean        -7.903  9.218
# std dev.   23.137  6.261

#significant (t(18)=-2.259  , p < 0.037, CI = [-33.045, -1.196] , d = 1.01),


# Second repetition: when mean, sd = same, sample size = different 


first_tutor_grades = rnorm(n = 100, mean = 0, sd = 10)
first_tutor = rep(c("tutor1"),each=100)
data_frame = data.frame(first_tutor,first_tutor_grades)
colnames(data_frame) = c("tutor", "score")



second_tutor_grades = rnorm(n = 10, mean = 10, sd = 25)
second_tutor_grades
second_tutor <- rep(c("tutor2"),each=10)
data_frame2 <- data.frame(second_tutor, second_tutor_grades)
colnames(data_frame2) <- c("tutor", "score")



final_df= rbind(data_frame, data_frame2)
final_df
final_df$tutor = factor(final_df$tutor)
independentSamplesTTest(formula = score ~ tutor, data = final_df, var.equal = TRUE)


#           tutor1 tutor2
# mean     -1.857 -1.968
# std dev. 10.229 23.391

##significant (t(108)=0.028   , p < 0.978 , CI = [-7.709, 7.931]  , d = 0.009),




# Third repetition: when sample size, sd = same, mean = different 


first_tutor_grades = rnorm(n = 10, mean = 10, sd = 10)
first_tutor = rep(c("tutor1"),each=10)
data_frame = data.frame(first_tutor,first_tutor_grades)
colnames(data_frame) = c("tutor", "score")



second_tutor_grades = rnorm(n = 10, mean = 7, sd = 25)
second_tutor_grades
second_tutor <- rep(c("tutor2"),each=10)
data_frame2 <- data.frame(second_tutor, second_tutor_grades)
colnames(data_frame2) <- c("tutor", "score")



final_df= rbind(data_frame, data_frame2)
final_df
final_df$tutor = factor(final_df$tutor)
independentSamplesTTest(formula = score ~ tutor, data = final_df, var.equal = TRUE)

#           tutor1 tutor2
# mean      3.003 14.453
# std dev. 10.960 20.558

# significant (t(18)=-1.554    , p < 0.138  , CI = [-26.928, 4.028]   , d = 0.695 ),



