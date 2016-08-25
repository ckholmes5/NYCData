library(MASS)

#Load the cats dataset from the MASS library. This dataset includes the body and heart weights of both male and female adult domestic cats.
data(cats)

#1. Create a scatterplot of heart weight versus body weight. From this plot alone, do you think simple linear regression would be a good fit for the data? Why?
plot(cats$Bwt, cats$Hwt )
#I think a linear regression would be a good fit for this data, because it seems like there is a consistent, linear relationship between weight and height. 

#2. Regress heart weight onto body weight. For this model:
model = lm(Hwt ~ Bwt, data = cats)
summary(model)

#a. Write out the regression equation.
# heart_weight = -.3597 + body_weight*4.034

#b. Interpret the meaning of the coefficients in context of the problem.
#The intercept means that a cat weighing nothing would have a negative sized heart. This doesn't make much sense.
#The beta1 intercept indicates that for every pound that the cat gains, its heart size would grow by 4.034 ounces (assuming ounces, could be grams?)

#c. Are the coefficients significant? How can you tell?
#The beta1 coefficient is significant, as its t-value is below the minimum possible t-value that R can calculate. The intercept is not significant.

#d. Is the overall regression significant? How can you tell? How does the answer to part c. relate?
#The overall regression is significant, because the f-stat has a tiny p-value. It is the square of the beta on the weight variable in the mdoel. 

#e. Find and interpret the RSE.
#The RSE is 1.452, which is how much the residuals deviate around the regression line, which means that the predictions are off by 1.452 ounces, on average.

#f. Find and interpret the coefficient of determination.
#The coefficient of determination is .65, meaning 65% of the variabiliy in heart weight is explained by the model.

#3. Add the regression line to your plot from part 1.
plot(cats$Bwt, cats$Hwt)
abline(model)

#4. Add a visualization of the residuals to your plot from part 3. Do any of the residuals seem abnormally large?
plot(model$fitted, model$residuals,
     xlab = "Fitted Values", ylab = "Residual Values",
     main = "Residual Plot for Cats Dataset")
abline(h = 0, lty = 2)
#The values towards the right side of the data set seem fairly large.


#5. Construct 95% confidence intervals for the model coefficients. Interpret the intervals in context of the problem.
confint(model)

#2.5 %   97.5 %
#(Intercept) -1.725163 1.011838
#Bwt          3.539343 4.528782

#The confidence interval around the intercept beta means that we are 95% confident that a cat that weighs nothing will have a heart size between -1.72 ozs and 1.01 ozs. Since a cat that weighs nothing will have a heart that also weighs nothing (because the cat does not exist), this makes sense in the context of our data.
#The confidence interval on bwt indicates that we are 95% confident that every pound that a cat increases in weight results in an increase in 3.5 to 4.5 ozs in heart weight.

#6. Assess each of the assumptions of the model.
plot(model)

#Linearity: From the graph, there is clearly a linear relationship.
#Normality of residuals: It does appear that the residuals are normally distributed.
#Constant Variance: They vary randomly throughout the dataset.
#Independent Events: THere appears to be no relationship between the data points.

#7. Redraw the scatterplot and regression line from part 3 and add both confidence and prediction bands.
newdata = data.frame(Bwt = 2:4)
plot(cats$Bwt, cats$Hwt)
conf.band = predict(model, newdata, interval = "confidence")
pred.band = predict(model, newdata, interval = "prediction")
conf.band = predict(model, newdata, interval = "confidence")

lines(cats$Hwt, conf.band[, 2], col = "blue") #Plotting the lower confidence band.
lines(cats$Hwt, conf.band[, 3], col = "blue") #Plotting the upper confidence band.

#8. Construct confidence and prediction intervals for body weights of 2.8 kg, 5 kg, and 10 kg. Do you foresee any issues with reporting any of these intervals?

#Question #2: Considering Transformations
#1. Create a Box-Cox plot for transforming the catsregression you created in question 1 above.
bc = boxcox(model)

#2. Determine the best value of lambda for a Box-Cox transformation on the cats regression. (Hint: Try to balance interpretability and accuracy; when taking this perspective, there isn’t really a completely “correct” answer.)
lambda = bc$x[which(bc$y == max(bc$y))] 

#3. Transform your data based on your answer to part 2.
bc_new = (cats$Hwt^lambda - 1)/lambda

#4. Construct a new regression now using the transformed data.
new_model = lm(bc_new ~ cats$Bwt)

#5. Create a scatterplot of the transformed data and overlay the new regression line.
plot(bc_new, cats$Bwt)

#6. Inspect the summary information and validate the assumptions of the linear regression model. Is there anything to be concerned about in the new model?
plot(new_model)

#7. Compare the models you created:
summary(model)
summary(new_model)
#  a. Give one reason why you might use the original model instead of the Box-Cox transformed model.
#The adjusted R-squared is lower in the new model, thus more of the variance is unexplained.

#b. Give one reason why you might use the Box-Cox transformed model instead of the original model.
#Normality is reduced so the regression is less biased.

#8. Attempt to apply a Box-Cox transformation on the model on which you already applied a Box-Cox transformation. What happens?
boxcox(new_model)
#One falls within the confidence interval, meaning we can't reject doing nothing to the adjust the data.