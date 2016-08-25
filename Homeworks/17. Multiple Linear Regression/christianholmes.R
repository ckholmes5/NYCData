#Question 1
restaurants = read.csv("/Users/cholmes/Downloads/MultipleLinearRegressionHomework/[04] NYC Restaurants.txt", sep=" ")

# 1. Create a scatterplot matrix of all continuous variables colored by Location. From this plot alone, do you see any problems that might arise for multiple linear regression?
plot(restaurants, col = restaurants$Location)
#Several variables are correlated, which could lead to collinearity.

# 2. Fit a multiple linear regression predicting the price of a meal based on the customer views and location of the restaurant. For this model:
# a. Write out the regression equation.
model = lm(Price ~ . - Restaurant, data = restaurants)


# b. Interpret the meaning each of the 5 coefficients in context of the problem.
summary(model) 
#For every increase in a point in the average food score, the restaurant's price increases by an average of $1.54.
#For every increase in a point in the average decor score, the restaurant's price increases by an average of $1.91.
#For every increase in a service point, the price decreases by an average of .2 cents.
#If the restaurant is on the East side of town, the price increases by an average of $2.07.

# c. Are the coefficients significant? How can you tell?
# The coefficents Food and Decor are significant at any level. Location is significant at the .05 level, however not at any level higher than that. Service is not significant.

# d. Is the overall regression significant? How can you tell?
# Yes, the F-statistic is highly significant at any level.

# e. Find and interpret the RSE.
# D.D. - ?

# f. Find and interpret the adjusted coefficient of determination.
# The adjusted R^2 is .6187, which means that 61.87% of the price variable is explained by this model.

# 3. Investigate the assumptions of the model using the plot()function. Are there any violations?
plot(model)

# Linearity: There seems to be a linear relationship.
# Normality: The residuals appear to be normal, besides perhaps at the very end of the right tail.
# Constant Variance: Variance seems to be constant
# Independent Errors: The errors appear to be independent.

# 4. Investigate the influence plot for the model. Are there any restaurants about which we should be concerned?
influencePlot(model)

# Restaurants 130 and 56 both seems to have a large influence on the model, however there don't seem to be any concerning data points.

# 5. Investigate the coefficient variance inflation factors; use these values to discuss multicollinearity.


# 6. Create added variable plots for this model. What conclusions might you draw from these plots?
avPlots(model) 
#Service might be collinear with food and decor, as it is added after the other two yet has no relationship w/ Price.

# 7. Fit a new simple linear regression that predicts the price of dinner from the service rating alone. Discuss this regression in light of your answer to part 6.
model = lm(Price ~ Service, data = restaurants)
summary(model) 
#Service is highly significant in its influence on Price in this model. This indicates that Service is collinear with Food or Decor (or both of them.)

#Question 2
# 1. Regress the price of dinner onto the average customer food rating, decor rating, and the restaurant location. In context of this new model, comment on:
model_new= lm(Price ~ . - Service - Restaurant, data = restaurants)
# a. The model summary() output.
summary(model_new)
#Both Food and Decor are now more signifcant than they were in the previous model. This is due to the variance decreasing because service was taken out for being collinear.

# b. The assumptions of multiple linear regression.
plot(model_new)
# Linearity: Appears to be linearly related.
# Normality: Residuals are normally scattered, aside from the far right tail of the model
# Constant Variance: Residuals appear to have constain variance.
# Independent Errors: Errors appear to be independent.

# c. The influence plot of the model.
influencePlot(model_new)
#56 and 130 still seem to have a large influence on the model, but there doesn't seem to be much to worry about here.

# d. The variance inflation factors of the coefficients.
vif(model_new)

# e. The added variable plots for the model.
avPlots(model_new) 
#There doesn't appear to be any significant collinear problems now.

# 2. Run a partial F-test to compare this model with the overall model you created in question 1. Interpret your results.
anova(model, model_new)
# The service variable is not informative for this model, so we'll exclude it going forward.

# 3. Fit a new reduced model that predicts the price of dinner by only the average customer food rating and average customer decor rating. Briefly comment on the model assumptions.
model_reduced = lm(Price ~ Food, Decor, data = restaurants)

#This is assuming that Food and Decor are not collinear and that Location and Service don't add anything meaningful to the model.

# 4. Compare each of the following models based on AIC:
AIC(model, model_reduced, model_new)

# 5. Compare each of the models based on BIC.
BIC(model, model_reduced, model_new)

# 6. Do you expect to see the results from part 4 and part 5? Which model would you ultimately choose to use?
#These models are descriptive, so I would go for the best BIC model (model_reduced).

# D.D. - Good job!