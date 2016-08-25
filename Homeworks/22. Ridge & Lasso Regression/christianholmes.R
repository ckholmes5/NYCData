library(glmnet)
#Question #2: Ridge Regression
#Read in the  [07] Prostate.txt dataset into your workspace. This dataset comes from a study by Stamey et a. (1989) of prostate cancer, measuring the correlation between the level of a prostate-specific antigen and some covariates. The included variables are the log-cancer volume, log-prostate weight, age of patient, log-amount of benign hyperplasia, seminal vesicle invasion, log-capsular penetration, Gleason score, and percent of Gleason scores 4 or 5; the response variable is the log-psa.
data = read.table('/Users/cholmes/Downloads/[07] The Curse of Dimensionality Homework (2)/[07] Prostate.txt')

#1. Create a training set of approximately 80% of your data and a test set of approximately 20% of your data ( NB: Use  set.seed(0) so your results will be reproducible.)
set.seed(0)
x = model.matrix(lpsa ~ ., data)[, -1]
y = data$lpsa
train = sample(1:nrow(data), 8*nrow(data)/10)
test = (-train)
y.test = y[test]


#2. Fit a slew of ridge regression models  on your training data by checking across a wide range of lambda values. Save the coefficients of these models in an object.
grid = 10^seq(5, -2, length = 100)
cv.ridge.out = cv.glmnet(x[train, ], y[train], lambda = grid, alpha = 0)


#3. Plot the coefficients of these models and comment on the shrinkage.
plot(cv.ridge.out)
#It appears to bottom out at somewhere around lambda=-2

#4. Perform 10-fold cross-validation  on your training data and save the output as an object. Once again, use  set.seed(0)  . ( NB: You can manually define the values of lambda to as you did in part 2).
set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train], lambda = grid, alpha = 0, nfolds = 10)

#5. Create and interpret a plot associated with the 10-fold cross-validation completed in part 4.
plot(cv.ridge.out)

#6. What is the best lambda?
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
log(bestlambda.ridge)
# best lambda = 0.1353048
# log(bestlambda.ridge) = -2.000225

#7. What is the  test  MSE associated with this lambda value?
ridge.lambdabest = predict(cv.ridge.out, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.lambdabest - y.test)^2)
#0.4913108

#8. Refit the ridge regression using the best lambda using every observation in your original dataset . Briefly comment on the coefficient estimates.
ridge.all = glmnet(x, y, lambda = bestlambda.ridge, alpha = 0)
predict(ridge.all, s = bestlambda.ridge, type = 'coefficient')


#9. What is the overall MSE for the model you fit in part 8? How does this compare to the MSE you found in part 7?
ridge.overall.predicted = predict(ridge.all, s = bestlambda.ridge, newx = x)
mean((ridge.overall.predicted - y)^2)
#MSE8= 0.4549825
#MSE7 = 0.4913108
#MSE8 is lower than MSE7, as it using the all of the data to predict this time.

#Question #3: Lasso Regression
#Continue using the  [07] Prostate.txt dataset you already loaded into your workspace.
#1. Repeat the entire analysis performed in question #2, except use the method of lasso regression instead.
set.seed(0)
train = sample(1:nrow(data), 8*nrow(data)/10)
test = (-train)
y.test = y[test]
x = model.matrix(lpsa ~ ., data)[, -1]
y = data$lpsa

grid = 10^seq(5, -2, length = 100)
cv.ridge.out = cv.glmnet(x[train, ], y[train], lambda = grid, alpha = 1)


plot(cv.ridge.out)
#It appears to bottom out at somewhere around lambda=-2

set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train], lambda = grid, alpha = 1, nfolds = 10)

plot(cv.ridge.out)

bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
log(bestlambda.ridge)
# best lambda = 0.0367838
# log(bestlambda.ridge) = -3.302698

ridge.lambdabest = predict(cv.ridge.out, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.lambdabest - y.test)^2)
#0.5060285

ridge.all = glmnet(x, y, lambda = bestlambda.ridge, alpha = 1)
predict(ridge.all, s = bestlambda.ridge, type = 'coefficient')


ridge.overall.predicted = predict(ridge.all, s = bestlambda.ridge, newx = x)
mean((ridge.overall.predicted - y)^2)
#MSE8= 0.4599623
#MSE7 = 0.5060285
#I would use the ridge regression method, as it improves accuracy in its predictions.

