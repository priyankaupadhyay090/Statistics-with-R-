##########################
#Week 11: Model Families and Logistic Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students


## Please write below your (and your teammates) name, matriculation number. 



## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

######################################################################################################################


####
#Part 1
####
# The folder speed.dating
# contains data from an experiment on a few hundred students that randomly assigned
# each participant to 10 short dates with participants of the opposite sex
# (Fisman et al., 2006). For each date, each person recorded several subjective
# numerical ratings of the other person (attractiveness, compatibility, and some 
# other characteristics) and also wrote down whether he or she would like to meet
# the other person again. Label and rij1, . . . , rij6 as person i’s numerical ratings of person j 
# on the dimensions of attractiveness, compatibility, and so forth.






#(1) Fit a classical logistic regression predicting Pr(yij = 1) given person i's 
#    ratings of person j. For ratings, use the features attr, sinc, intel, fun; see the documentation for what exactly these
#    abbreviations stand for.
#    Also, please plot the data in order to inspect it, and discuss the importance of attractiveness, compatibility, and so 
#    forth in this predictive model.

library(ggplot2)
library(lme4)
dat = read.csv("Speed Dating Data.csv")

#first we convert dec to a factor
dat$dec <- factor(dat$dec)

#then we plot the decision as a function of attractiveness, sincerity, intelligence and fun and add some jitter
#in order to better see the effects
ggplot(data=dat, aes(y = dec_o, x= attr_o))+ 
  geom_jitter(position=position_jitter(0.1))
#This plot shows us that people perceived as attractive were more likely to be asked out after speed dating.

ggplot(data=dat, aes(y = dec_o, x= sinc_o))+ 
  geom_jitter(position=position_jitter(0.1))
#This plot shows that fro people perceived as rather sincere or very sincere the decision could be either way. There does not
#seem to be much of a difference. For people perceived as insincere there seems to be a slightly higher chance that the decision is not.


ggplot(data=dat, aes(y = dec_o, x= intel_o))+ 
  geom_jitter(position=position_jitter(0.1))
#Here we have very very similar results as with sincerity.


ggplot(data=dat, aes(y = dec_o, x= fun_o))+ 
  geom_jitter(position=position_jitter(0.1))
#The plot shows that people who are perceived as not funny, are more likely to end up without a date invitation. 
#People who are perceived as funny seems to have a little higher chances to be asked out but it does not seem like there is 
#a big effect on the decision.

#Overall the plots show that the attributes attractiveness and fun will probably be the once making the 
#decision, with attractiveness beeing the most important.

#we fit the model
mylogit <- glm(dec ~ attr_o + sinc_o + intel_o + fun_o, data = dat, family = "binomial")
summary(mylogit)

#All predictors are statistically significant, as we can see from the p-values.
#The estimated coefficients of the  model can be interpreted as follows:
#
# + Intercept = -1.00146: since the decision cannot be negative, the value of the
# intercept does not have an interpretation
#
# + attr_o = -0.15624: For each unit change in the attractiveness score, the log odds of the 
#decision being yes decrease by -0.15624. Which is very interesting because it is not what the plot above showed us. 
#
# + sinc_o = 0.07992: For each unit change in the sincerity score, the log odds of the 
#decision being yes increase by 0.07992.
#
# + intel_o = 0.09020: For each unit change in the intelligence score, the log odds of the 
#decision being yes increase by 0.09020.
#
# + fun_o = 0.06945: For each unit change in the fun score, the log odds of the 
#decision being yes increase by 0.06945.
#
#We find that the attractiveness score seems to have a much bigger effect on the decision than other coefficients. 
#It also seems to have a negative effect, while the others have a positive one. 
#This may be because many people are scared to ask out an attractive person, but the other attributes are not intimidating.
#However, we have not distinguished between the subjects scoring yet and maybe the above statement is not true if the person
#scoring is attractive or just very confident.






#(2) Expand this model to allow varying intercepts for the persons making the
#    evaluation; that is, some people are more likely than others to want to meet
#    someone again. Discuss the fitted model.

mylogit_2 = glmer(dec ~ attr_o + sinc_o + intel_o + fun_o + (1|pid), family=binomial(logit), data=dat)
summary(mylogit_2)

#The estimated coefficients of the  model can be interpreted as follows:
#
# + Intercept = -1.26109: since the log odds of the decision cannot be negative, the value of the
# intercept does not have an interpretation
#
# + attr_o = -0.14295: For each unit change in the attractiveness score, the log odds of the 
#decision being yes decrease by -0.14295.
#
# + sinc_o = 0.08778: For each unit change in the sincerity score, the log odds of the 
#decision being yes increase by 0.08778.
#
# + intel_o = 0.08932: For each unit change in the intelligence score, the log odds of the 
#decision being yes increase by 0.08932.
#
# + fun_o = 0.08173: For each unit change in the fun score, the log odds of the 
#decision being yes increase by 0.08173.
#
#The coefficients are similar to the previous ones. However, we can see that now that we have attributed for the person 
#scoring, the attractiveness and the the intelligence have a slightly smaller effect, while the sincerity and the fun a 
#slightly bigger one.










#(3) Expand further to allow varying intercepts for the persons being rated. Discuss
#    the fitted model. Interpret the model outcome and explain what the varying intercepts are.

mylogit_3 = glmer(dec ~ attr_o + sinc_o + intel_o + fun_o + (1|pid) + (1|iid), family=binomial(logit), data=dat)
summary(mylogit_3)


#The intercepts show us that there is less variation in the person scoring than in the person 
#being scored. So there is a bigger difference when comparing the scores for different people (who are being scored), then
#when comparing all scores given to a single person by different people.

#The estimated coefficients of the  model can be interpreted as follows:
#
# + Intercept = -2.31992: since the log odds of the decision cannot be negative, the value of the
# intercept does not have an interpretation.
#
# + attr_o = -0.04922 : For each unit change in the attractiveness score, the log odds of the 
#decision being yes decrease by -0.04922.
#
# + sinc_o = 0.06885: For each unit change in the sincerity score, the log odds of the 
#decision being yes increase by 0.06885.
#
# + intel_o = 0.09748: For each unit change in the intelligence score, the log odds of the 
#decision being yes increase by 0.09748.
#
# + fun_o = 0.15218: For each unit change in the fun score, the log odds of the 
#decision being yes increase by 0.15218.
#
#When also attributing for the difference between the people who are being scored, we see that the attractiveness has
#a much, much smaller effect and fun a much, much bigger one, than in the previous models. Sincerity and intelligence 
#remain more or less the same. In this model, fun is the most important factor.









#(4) Now fit some models that allow the coefficients for attractiveness, compatibility, and the 
#    other attributes to vary by person.  Fit a multilevel model, allowing the intercept and the 
#    coefficients for the 6 ratings to vary by the rater i. (Hint: The model will not converge when you 
#    include many predictors as random slopes; see with how many predictors you can get the model to converge;
#    and try out some of the tricks we have seen to see whether they affect convergence for this dataset.)


mylogit_4 = glmer(dec ~ attr_o + sinc_o + intel_o + fun_o + (1+attr_o|iid) , family=binomial(logit), data=dat)
summary(mylogit_4)


mylogit_5 = glmer(dec ~ attr_o + sinc_o + intel_o + fun_o + (1+sinc_o|iid) , family=binomial(logit), data=dat)
summary(mylogit_5)


mylogit_6 = glmer(dec ~ attr_o + sinc_o + intel_o + fun_o + (1+intel_o|iid) , family=binomial(logit), data=dat)
summary(mylogit_6)


mylogit_7 = glmer(dec ~ attr_o + sinc_o + intel_o + fun_o + (1+fun_o|iid) , family=binomial(logit), data=dat)
summary(mylogit_7)


mylogit_8 = glmer(dec ~ attr_o + sinc_o + intel_o + fun_o + (1+attr_o|pid), family=binomial(logit), data=dat)
summary(mylogit_8)

mylogit_9 = glmer(dec ~ attr_o + sinc_o + intel_o + fun_o + (1+attr_o||pid) + (1+intel_o||pid)  + (1+fun_o||pid), family=binomial(logit), data=dat)
summary(mylogit_9)




#(5) compare the output for the different models that you calculated - did the model design affect your conclusions?

#In model mylogit_4, we have a random intercept and slope for attractiveness per person. This changes our conclusion. 
#Now the attractiveness again a very important factor. However, fun is still the most important one as in mylogit_3.

#In model mylogit_5, we have a random intercept and slope for sincerity per person. 
#The conclusion is again that fun is the most important factor before attractiveness.

#In model mylogit_6, we have a random intercept and slope for intelligence per person. 
#The conclusion is the same as for mylogit_5.

#In model mylogit_7, we have a random intercept and slope for fun per person.
#The conclusion is that fun is the most important factor before attractiveness. 


#The multi-level model (mylogit_8) does not even converge with one random intercept and slope. Therefore we use a trick and
#change the model to mylogit_9. With this trick the model converges with at most 3 random intercepts and slopes. 
#The coefficients of this model show that attractiveness is by far the most important factor, followed by intelligence, 
#sincerity and fun. The fact that now fun is the least important factor shows again that the model design highly affects the 
#conclusion.










####
#Part 2
####

# In this example, num_awards is the outcome variable and indicates the number of awards earned by students at
# a high school in a year, math is a continuous predictor variable and represents students' scores on their 
# math final exam, and prog is a categorical predictor variable with three levels indicating the type of program 
# in which the students were enrolled. It is coded as 1 = "General", 2 = "Academic" and 3 = "Vocational". 
# Let's start with loading the data and looking at some descriptive statistics.

p = read.csv("poisson_sim.csv", sep=";")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(id)
})
summary(p)



#(6) Plot the data to see whether program type and math final exam score seem to affect the number of awards.

ggplot(data=p, aes(y=num_awards, x=math, color = prog))+
  geom_point()

#It seems that both variables have an effect on the number of awards.




#(7) what model family is used and explain the reason for using it.
#Run a generalized linear model to test for significance of effects.

#We use a poisson model, because the residuals have a poisson distribution: there is a tail to the left, 
#but no to the right.

glm1 = glm(num_awards~math+prog,family=poisson, data=p)
summary(glm1)

#The variable math seems to be significantly significant. This is also the case for prog but only for
#the Academic value: so it is only important whether the student has a Academic education or not. In the case that he has not,
#it does not matter whether his education is General or Vocational.





#(8) Do model comparisons to find out whether the predictors significantly improve model fit.

glm2 = glm(num_awards~math,family=poisson, data=p)
summary(glm2)

glm3 = glm(num_awards~prog,family=poisson, data=p)
summary(glm3)

#The AIC values of the 3 models show that adding math to the predictors improves the model significantly (416.51 to 373.5), 
#while adding prog improves only slightly (373.5 to 384.08). This makes sense, because as we can see from the plot, almost 
#all students who have a high math score, have Academic prog education. So there is not much  additional information 
#gained by adding prog.






#(9) Compare to a model that uses a gaussian distribution (normal lm model) for this data.

glm4 = glm(num_awards~math+prog, data=p)
summary(glm4)

#The AIC value (532.25) shows us that this model fits the data worse than all models before.






##Task 3

## Please explain within and between subject experimental design.
##How does the design affect the random effect structure during analysis.

#In a between subject experimental design, we get only measurement from each subject and they are all exposed to only 
#one item/condition.
#We are then interested in the differences between the different subjects.

#In a within subject design, we get more than one measurement from each subject, because they are all exposed to more than one
#condition/item.
#We are then interested in the differences between the measurements of one subject only. So for each subject we investigate, 
#whether there is a difference between the different measurements we have from him/her. But we do not compare measurements from
#different subjects to each other.

#During analysis it is important to know which design we have, so that we know whether we need random intercepts and/or 
#random slopes.



