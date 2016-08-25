library(Sleuth2)

#Question #1: Birdkeeping & Lung Cancer
#Load the Sleuth2 library and extract the case2002 dataset. This dataset reports results of a survey conducted from 1972 to 1981 in the Netherlands aiming to see if birdkeeping is a risk factor for lung cancer. Variables include whether or not an individual had lung cancer, whether or not they were birdkeeping, their gender, socioeconomic status, age, years of smoking, and average rate of smoking.
data = data(case2002)
#1. Perform some basic numerical and graphical EDA. In particular, comment on the scatterplots of the continuous variables colored by whether or not an individual had lung cancer. What might be good? What might be bad?
plot(case2002)
summary(case2002)
#Age and years of smoking appear to be linked.

#2. Fit a logistic regression predicting whether or not an individual has lung cancer that includes all variables in the model.
logit_model = glm(LC ~ FM + SS + BK + AG + YR + CD, family = 'binomial', data = case2002)

#3. Briefly assess the appropriate residual plot and an influence plot for the model created in part 2.
influencePlot(logit_model)
scatter.smooth(logit_model$fit, residuals(logit_model, type = "deviance"))

#4. Conduct and interpret an overall goodness of fit test for the model created in part 2.
summary(logit_model)
#AIC is 168.2

#5. Interpret the coefficient of gender on the log odds scale.
#The log odds of someone in this sample for getting lung cancer decreases by 56% if the person is male. This beta is not significant at the .05 level, though.

#6. Interpret the coefficient of socioeconomic status on the odds scale.
#The log odds of someone in this sample getting lung cancer decreases by 10% if they are in the lower class.

#7. Interpret the 95% confidence interval based on standard errors for the birdkeeping indicator on the log odds scale.
#confint(logit_model)
#The confidence interval indicates that we are 95% confident that the true change in the probability of getting lung cancer decreases by between 219% and 57% if you don't have a bird.

#8. Interpret the 95% confidence interval based on standard errors for the years of smoking variable on the odds scale.
#We are 95% confident that for every year you smoke, your risk of lung cancer increases by 2%-13%

#9. Fit a logistic regression predicting whether or not an individual has lung cancer that includes all variables in the model except the birdkeeping indicator.
logit_model_nobirds = glm(LC ~ FM + SS + AG + YR + CD, family = 'binomial', data = case2002)


#10. Conduct and interpret an overall goodness of fit test for the model created in part 9.
summary(logit_model_nobirds)

#11. Conduct and interpret a drop in deviance test comparing the two models you’ve created thus far. Which would you keep in favor of the other?
anova(logit_model , logit_model_nobirds, test = "Chisq")
#I would keep no birds because it has a higher deviance.

#12. Fit a logistic regression predicting whether or not an individual has lung cancer based only on whether or not they have birds and the number of years they have been smoking.
logit_model_simple = glm(LC ~ BK + YR, family = 'binomial', data = case2002)


#13. Conduct and interpret a drop in deviance test comparing the model you created in part 12 to the model you created in part 2. Which would you keep in favor of the other?
anova(logit_model , logit_model_simple, test = "Chisq")


#14. Compare the models across:
#  a. AIC
AIC(logit_model,logit_model_nobirds,logit_model_simple)


#  b. BIC
BIC(logit_model,logit_model_nobirds,logit_model_simple)

#d. Give an argument for choosing the model created in part 12.
#It gets the best BIC score, which indicates it's a good descriptive model.

#15. Using the model created in part 12, predict:
#  a. The probability of having lung cancer for an individual with an average number of years smoking with and without birds within their household.
#Probability Lung cancer with bird:
exp(-1.70460 + .058*mean(case2002$YR))/(1+exp(-1.70460 + .058*mean(case2002$YR)))

#Probability Lung Cancer with no bird
exp(-1.70460 + .058*mean(case2002$YR) - 1.47555)/(1+exp(-1.70460 + .058*mean(case2002$YR) - 1.47555))

#b. The probability of having lung cancer for an individual with no years prior smoking with and without birds within their household.
#With bird
exp(-1.7046)/(1+exp(-1.7046))

#Without Bird
exp(-1.7046 + -1.4755)/(1+exp(-1.7046 + -1.4755))

#16. Use the model created in part 12 to classify the observations in your dataset as having or not having lung cancer. Comment on how well the model performs as compared to the baseline.
bird_predict = function(years, bird) {
  prob = exp(years * 0.05825 - 1.70460 + 1.4755 * bird)/(1+ exp(years * 0.05825 - 1.70460 + 1.4755 * bird))
  prob
}

lc_predict = mutate(case2002, prediction = 0, correct = 0, LC_bin = ifelse(LC == 'LungCancer', 1,0), birdstatus = ifelse(BK == 'Bird',1,0))
lc_predict$prediction = ifelse(bird_predict(lc_predict$YR,lc_predict$birdstatus) > .5, 1,0)
lc_predict$correct = ifelse(lc_predict$prediction == lc_predict$LC_bin, 1, 0)
sum(lc_predict$correct)/147
#.5782313

#Baseline:
mean(lc_predict$LC_bin)
#.3333333
#If you just guessed that everyone doesn't have lung cancer, you'd get it right 66.6% of the time. So the model fails.









