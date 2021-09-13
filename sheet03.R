### Stats with R Exercise sheet 3

##########################
#Tests for Categorical Data
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, November 30th. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 



## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

## Only 1 member needs to submit! 

## It is better if you do the datacamp exercise first on ggplot! It will help you in the assignment. 

#################################################################################
#################################################################################


###############
### Exercise 1: Plotting graphs using ggplot.
###############
# There are many ways of making graphs in R, and each has their own advantages 
# and disadvantages. One popular package for making plots is ggplot2. 
# The graphs produced with ggplot2 look professional and the code is quite easy 
# to manipulate.
# In this exercise, we'll plot a few graphs with ggplot2 to show its functionalities.
# You'll find all the information you'll need about plotting with ggplot2 here: 
# http://www.cookbook-r.com/Graphs/


# Also, you have been assigned the ggplot2 course in DataCamp. Please work through 
# this course first to easily solve the assignment below

## a) First install and load the ggplot2 package. Look at the help for ggplot.
install.packages("ggplot2")
library(ggplot2)
library(languageR)




## b) We're going to be plotting data from the dataframe 'ratings' 
##    (included in languageR). 
##    Look at the description of the dataset and the summary.
?ratings
summary(ratings)






## For each word, we have three ratings (averaged over subjects), one for the 
## weight of the word's referent, one for its size, and one for the words' 
## subjective familiarity. Class is a factor specifying whether the word's 
## referent is an animal or a plant. 
## Furthermore, we have variables specifying various linguistic properties, 
## such as word's frequency, its length in letters, the number of synsets 
## (synonym sets) in which it is listed in WordNet [Miller, 1990], its 
## morphological family size (the number of complex words in which 
## the word occurs as a constituent), and its derivational entropy (an 
## information theoretic variant of the family size measure). 
## Don't worry, you don't have to know what all this means yet in order to 
## be able to plot it in this exercise!

## c) Let's look at the relationship between the class of words and the length. 
##    In order to plot this, we need a dataframe with the means.
##    Below you'll find the code to create a new dataframe based on the existing 
##    dataset ratings.
##    Plot a barplot of ratings.2 using ggplot. Map the two classes to two 
##    different colours. 
##    Remove the legend.
summary(ratings)
condition <- c("animal", "plant")
frequency <- c(mean(subset(ratings, Class == "animal")$Frequency), mean(subset(ratings, Class == "plant")$Frequency))
length <- c(mean(subset(ratings, Class == "animal")$Length), mean(subset(ratings, Class == "plant")$Length))
ratings.2 <- data.frame(condition, frequency, length)
ratings.2

ggplot(ratings.2, aes(x=condition, y=length, fill=condition))+
  geom_bar(stat="identity")+
  theme(legend.position="none")

  




## d) Let's assume that we have additional data on the ratings of words. 
##    This data divides the conditions up into exotic and common animals 
##    and plants.
##    Below you'll find the code to update the dataframe with this additional data.
##    Draw a line graph with multiple lines to show the relationship between 
##    the frequency of the animals and plants and their occurrence.
##    Map occurrence to different point shapes and increase the size 
##    of these point shapes.
condition <- c("animal", "plant")
frequency <- c(7.4328978, 3.5864538)
length <- c(5.15678625, 7.81536584)
ratings.add <- data.frame(condition, frequency, length)
ratings.3 <- rbind(ratings.2, ratings.add)
occurrence <- c("common", "common", "exotic", "exotic")
ratings.3 <- cbind(ratings.3, occurrence)
ratings.3


ggplot(ratings.3, aes(x=condition, y=frequency, group=condition)) +
  geom_line()+
  geom_point(aes(shape=occurrence, size=4))


## e) Based on the graph you produced in question d, 
##    what can you conclude about how frequently 
##    people talk about plants versus animals, 
##    with regards to how common they are?

#When it comes to animals people talk more about common animals and much less about uncommon ones. 
#When it comes to plants on the other hand, people talk more about uncommen ones than about common ones.






##########
##Exercise 2. Binomial distribution
##########
## Suppose there are 12 multiple choice questions in a quiz. 
## Each question has 4 possible answers, and only one of them is correct. 

## a) Please calculate the probability of getting exactly 5 answers right 
##    if you answer by chance. Calculate this using the dbinom() function.

help(dbinom)
dbinom(5, 12, 1/4)
#answer 0.1032414


## b) Next please calculate the probability of answering 4 or less questions 
##    correctly by chance. 

pbinom(4, 12, 1/4)
#answer 0.8423563




##########
##Exercise 3. Chi-square test
##########
## a) Consider the dataset dutchSpeakersDistMeta from sheet1. 
##    Load the package (languageR) and look at the summary of the variables, 
##    as well as their classes. Which variables are factors?

library(languageR) #however, package is already loaded above :)
summary(dutchSpeakersDistMeta)
class(dutchSpeakersDistMeta$Speaker)
class(dutchSpeakersDistMeta$Sex)
class(dutchSpeakersDistMeta$AgeYear)
class(dutchSpeakersDistMeta$AgeGroup)
class(dutchSpeakersDistMeta$ConversationType)
class(dutchSpeakersDistMeta$EduLevel)

#All are factors but AgeYear.



## b) We want to find out whether there is a difference between males and females 
##    with respect to the age groups they are in.
##	  First use the function 'table()' to get the counts and create 
##    a contingency table of AgeGroup by Sex.

AgeSex = table(dutchSpeakersDistMeta$AgeGroup, dutchSpeakersDistMeta$Sex)


##    Visualize your data with a single bar plot (use ggplot) that represents the counts with 
##    respect to each age group and each sex.

ggplot(dutchSpeakersDistMeta, aes(AgeGroup, fill=Sex))+
  geom_bar()






## c) Inspect the table you created in b). Does it look like there could be a significant 
##    difference between the sexes?

#No, not really.




## d) We are going to calculate whether there's a difference between males and females 
##    regarding their age group using the function chisq.test. 
##    Look at the help of this function. 
##    Then use the  function to calculate whether there's a difference in our table from b). 
##    Is there a significant difference in age group?

help(chisq.test)
chisq.test(AgeSex)
#No, there is no significant difference since p-value 0.5124 > 0.05.




## e) What are the degrees of freedom for our data? How are they derived?

#We have 2 columns and 5 rows. So we calculate the degrees of freedom as follows:
# df= (2-1)*(5-1)= 4






##########
##Exercise 4. Binomial versus chi-square
########## 
##    In this exercise, we will consider a made up example of there doctors can predict  
##    if a patient has temperature or not just by holding their hand  
##    Several doctors were blindfolded and were asked to tell if the experimenter  
##    has temperature/fever or not.  
##    There were a total of 200  trials, of which the doctors 
##    correctly indicated that a patient had fever 83 times.

## a) What is the null hypothesis, i.e. how often would we expect the doctor to 
##    be correct by chance (in raw number and in percentage)?

#Null-hypothesis: This happended by chance and doctors cannot actually predict if a patient
#has fever just by holding there hand.
#If the null-hypothesis were true, we would expect the doctor to be correct 100 times (50%).





## b) Using a chisquare test, what do you conclude about whether this idea
##    of checking fever/temperature works? 


df = data.frame(c(83, 107))
chisq.test(df)
#answer:0.08166
#No, it does not work, because the probability of it happening by chance is too high.




## c) Now calculate significance using the binomial test as we used it in exercise 2.

pbinom(83, 200, 1/2)
#answer 0.009698472 
#Using this test, it seems that this this did not happan by chance and that the doctor can actually
#predict this correctly.




## d) The results from these two tests are slightly different. Which test do you think 
##    is better for our data, and why?

#I believe, that the binomial test is better because we actually have two possible outcomes.





##########
##Exercise 5.
##########
## Describe a situation where you would choose McNemar's test over the ChiSquare test. 
## What would be the problem of using the normal ChiSquare test in a case where 
## McNemar's test would be more appropriate?

#Example would be results of a study where we ask the subjects twice about something (back pain before and 
#after taking some medication). But because we are asking the same subjects twice, the answers are not 
#independent of each other. Therefor the ChiSpuare test is not appropiate. The McNemar's test, however, 
#does not assume that the values as sampled independently and can therefore be used.

