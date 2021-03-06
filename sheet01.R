### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, November 16. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#).
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates') name and matriculation number. 


## Change the name of the file by adding your matriculation numbers
## (sheet01_firstID_secondID_thirdID.R)



## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on  the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## moodle discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
getwd()

## b) Get help with this function.
help()

## c) Change your working directory to another directory.
setwd("/Users/cvete171/Documents/7. Semester")



###############
### Exercise 2: Participants' age & boxplots
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
install.packages("languageR")
library(languageR)







## b) Specifically, we will deal with the dataset 'dutchSpeakersDistMeta'. 
##    This dataset should be available to you once you've loaded languageR.
##    The dataset contains information on the speakers included in the Spoken 
##    Dutch Corpus. Inspect 'dutchSpeakersDistMeta'. Look at the head, tail, 
##    and summary. What do head and tail show you?

head(dutchSpeakersDistMeta)
tail(dutchSpeakersDistMeta)
summary(dutchSpeakersDistMeta)

#Head shows is the first six and tail the last six observations of the dataset.







## c) Each line in this file provides information on a single speaker. How many 
##    speakers are included in this dataset? In other words, use a function to 
##    retrieve the number of rows for this dataset.

nrow(dutchSpeakersDistMeta)

#There are 165 observations in the dataset.








## d) Let's say we're interested in the age of the speakers included in the 
##    corpus, to see whether males and females are distributed equally. 
##    Create a boxplot for Sex and AgeYear.

female = subset(dutchSpeakersDistMeta, Sex == "female")
male =  subset(dutchSpeakersDistMeta, Sex == "male")

boxplot(female$AgeYear,
        main = "AgeYear distribution for females",
        xlab = "AgeYear",
        ylab = "Female",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

boxplot(male$AgeYear,
        main = "AgeYear distribution for males",
        xlab = "AgeYear",
        ylab = "Male",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)






## e) Does it seem as if either of the two groups has more variability in age?

#Yes, the female group has more variability in age.









## f) Do you see any outliers in either of the two groups?

#Yes, there are outliers in the male group. 








## g) Now calculate the mean and standard deviation of the AgeYear per group. 
##    Do this by creating a subset for each group.
##    Do the groups seem to differ much in age?

mean(female$AgeYear) #1966.889
sd(female$AgeYear)   #15.87411

mean(male$AgeYear)   #1967.301
sd(male$AgeYear)     #14.66258

#The female group has a larger range for AgeYear.





## h) What do the whiskers of a boxplot mean?
  
#The upper whiskers display observations that are within the highest 25% of the dataset.
#However, the upper whisker can only be 1.5 times the length of the inter-quartile. So if there are some
# observations within twose upper 25% that would make the upper whisker larger than that, they are
#displayed separately as points above the upper whister and called outliers.

#The exact same goes for the lower whisker, but it displays the lowest 25% of the dataset and 
#potential outliers are displayed underneath it.
  
  
  



## i) What is the inter-quartile range in the boxplot?


#The inter-quartile starts at the lower quartile and ends at the upper quatile. So it corresponds to 
#the 50% of observations that are in the middle. 







## j) Is the plot positively or negatively skewed?

#Both boxplots (for females and for males) are positively skewed.







###############
### Exercise 3: Children's stories & dataframes
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 25 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 22 19 18 17 18 20 17 12 16 16 17 21 25 18 20 21 20 20 15 18 17 19 20 


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? (remember, comment out written answers)

#Discrete, because we cannot have values like 10.5 or 2.234.





## b) In the next questions (c-e), you will create a dataframe of this data, 
##    which will also include participant IDs.
##    Why is a dataframe better suited to store this data than a matrix?

#Because a matrix can only be used if all the data has the same class. However, in
# a later task we will create a factor from the IDs and then we will have two classes:
#factor and numeric.





## c) First create a vector with participant IDs. Your vector should be named 
##    'pps', and your participants should be labeled from 1 to 25

pps = seq(from = 1, to = 25, by = 1)







## d) Next, create a vector containing all the observations. Name this vector 'obs'.

obs = c(18, 15, 22, 19, 18, 17, 18, 20, 17, 12, 16, 16, 17, 21, 25, 18, 20, 21, 20, 20, 15, 18, 17, 19, 20 )
obs




## e) Create a dataframe for this data. Assign this to 'stories'. 

stories = data.frame(pps, obs)





## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class is the variable 'pps'?

summary(stories)
class(stories$pps)

#The class of 'pps' is numeric.





## g) Change the class of 'pps' to factor. Why is factor a better class for this
##    variable?


stories$pps <- as.factor(stories$pps)
summary(stories)

#Because IDs are unique and it does not make sense to calculate min, median, mean, max, etc.
#for them.









## h) Plot a histogram (using hist()) for these data. Set the number of breaks 
##    to 8.

hist(stories$obs, breaks = 8)








## i) Create a kernel density plot using density().

d <- density(stories$obs) 
plot(d)







## j) What is the difference between a histogram and a kernel density plot?

#A histogram is a bar chart that shows how often a value occures in a dataset.A density plot on the other
#hand, shows the density for the values of the dataset. It is a smooth curve.





## This is a difficult one, remember you just need to provide a serious attempt at solving each 
## exercise in order to pass. 
## k) Overlay the histogram with the kernel density plot 
##    (hint: the area under the curve should be equal for overlaying the graphs 
##    correctly.)

hist(stories$obs, prob = TRUE, breaks = 8)
lines(density(stories$obs))










###############
### Exercise 4: Normal distributions
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.1. Assign this to the 
##    variable x.


x = seq(from = -5, to = 5, by = 0.1)








## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.



y = dnorm(x)






## c) Now use plot() to plot the normal distribution for z values of "x". 



plot(x, y)




## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.8, and plot a line 
##    instead of the circles.

plot(x, y, type = "l", ylim = c(0, 0.8))








## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the median of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.

abline(v=mean(x), lty = 2)






## f) Take a look at the beaver1 dataset. (You can see it by typing "beaver1".) 
##    Then select only the temperature part and store it in a variable "b1temp".

b1temp = beaver1$temp
b1temp





## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.

m= mean(b1temp)
sd = sd(b1temp)
y = dnorm(b1temp, m, sd)
plot(b1temp, y)




## h) We observe two temperatures (36.91 and 38.13). What's the likelihood that
##    these temperatures (or more extreme ones) respectively come 
##    from the normal distribution from g)?

prob1 = 1 - (pnorm((36.91-m)/sd) )
prob2 = 1 - (pnorm((38.13-m)/sd) )





## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. What do you observe?

s1 = sample(y, size = 20)
hist(s1)

s2 = sample(y, size = 20)
hist(s2)

s3 = sample(y, size = 20)
hist(s3)

s4 = sample(y, size = 20)
hist(s4)

s5 = sample(y, size = 20)
hist(s5)

#We observe that all five histograms look completely different.




