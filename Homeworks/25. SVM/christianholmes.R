library(e1071)
#Question #1: Wine Quality
#Read in the  [10] Wine Quality.csvdataset into your workspace. The data contains 1,599 observations of red Vinho Verde wines from the north of Portugal. The goal is to model wine quality based on various physicochemical measurements.
df = read.csv('/Users/cholmes/Desktop/Work/Bootcamp/Homework/Homeworks/Support Vector Machines Homework/[10] Wine Quality.csv')

#1. Perform some data munging:
#a. Recode the  quality variable to be a factor variable with values of “Low” for quality ratings of 5 and below, and “High” for ratings of 6 and above.
df$quality = as.factor(ifelse(df$quality <= 5, 'Low', 'High'))

#b. Scale and center the numeric vectors of your dataset.
nums = df[,-12]
nums = as.data.frame(scale(nums))
df = cbind(nums, df[12])

#2. Split the data into a training and test set with an 80% - 20% split, respectively.
set.seed(0)
train_nums = sample(1:nrow(df), 8*nrow(df)/10)
test_nums = -train_nums
train = df[train_nums, ]
test  = df[test_nums, ]

#3. Briefly explore some graphical EDA:
plot_data = df[,-12]
plot(plot_data)

plot(df$fixed.acidity)
plot(df$volatile.acidity)
plot(df$citric.acid)
plot(df$residual.sugar)
plot(df$chlorides)
plot(df$free.sulfur.dioxide)
plot(df$total.sulfur.dioxide)
plot(df$density)
plot(df$pH)
plot(df$sulphates)
plot(df$alcohol)
plot(df$quality)

#a. Explain why a maximal margin classifier is impossible to fit to this data.
#Making a maximal margin classifier is contingent on a clear seperating hyperplane between all the variables. Since there are 11 varialbes, this is almost certainly not true.

#b. Explain why a support vector classifier is generally more desirable.
#The SVC has a soft margin, which allows for some overlap between the variable groups.

#4. Tune a support vector classifier with a cost ranging from 10^-5 to 10^5 
set.seed(0)
svc = tune(svm,quality ~ .,data = train,kernel = "linear",ranges = list(cost = 10^(seq(-5, .5, length = 50))))
summary(svc)

#a. What was the best cost parameter of the ones you tested?
#.5179

#b. What was the best error rate corresponding to the best cost?
#.2595

#c. Graphically view the cross-validated results. Is it plausible that you checked enough values of cost?
plot(svc)
#It looks like we hit a minimum around .2-.3, so it is plausible we checked enough values for cost.

#5. How many support vectors are there in your best support vector classifier?
best = svc$best.model
best
#There are 759 support vectors in this model.

#6. What is the test error associated with the best support vector classifier you found in part 4?
ypred = predict(best, test)
table("Predicted Values" = ypred, "True Values" = test$quality)
test_error = (134+105)/(134+105+37+44)
#0.746875

#7. Fit a support vector classifier to all of the data using the best cost parameter you found in part 4.
new_svc = svm(quality ~ .,
                    data = df,
                    kernel = "linear",
                    cost = 1)

#a. How many support vectors does this support vector classifier have?
new_svc
#946

#b. Is the 555th observation a support vector?
new_svc$index
#No! But 553 is.


#8. What is the overall error rate for the support vector classifier you created in part 7?
ypred_new = predict(new_svc, df)
table("Predicted Values" = ypred_new, "True Values" = df$quality)
(621 + 577) / (621 + 577 + 234 + 167)
#.7492

#9. Visualize the support vector classifier by examining the free sulfur dioxide and total sulfur dioxide cross-section; to do so, use the following line of code (modified with your object names):  plot(model, data, free.sulfur.dioxide ~ total.sulfur.dioxide). 
plot(new_svc, df, free.sulfur.dioxide ~ total.sulfur.dioxide)

#10. Tune a support vector machine with a radial kernel. Check both cost and gamma values using the following code snippets:  cost = seq(.75, 1.25, length = 5), gamma = seq(.55, .95, length = 5). C aution: This will take about a minute to run.
set.seed(0)
svm_rad = tune(svm, quality ~.,
              data = df,
              kernel = 'radial',
              cost = seq(.75, 1.25, length = 5),
              gamma = seq(.55, .95, length = 5)
)
 

#a. What was the best cost parameter of the ones you tested?
svm_rad = tune(svm, quality ~ ., data = train, kernel = "radial", ranges = list( cost = seq(.75, 1.25, length = 5),  gamma = seq(.55, .95, length = 5)))
summary(svm_rad$best.model)

#b. What was the best gamma parameter of the ones you tested?
#.85

#c. What was the best error rate corresponding to the best cost & gamma?
#1.25

#d. Graphically view the cross-validated results. Is it plausible that you checked enough values of cost and gamma?
plot(svm_rad$performances$cost,
     svm_rad$performances$error,
     type = 'l')
#It looks like the error rate will improve if we extend the graph to include more costs.

#11. How many support vectors are there in your best support vector machine?
#1305 support vectors

#12. What is the test error associated with the best support vector machine you found in part 10?
pred_rad = predict(svm_rad$best.model, test)
table("Predicted Values" = pred_rad, "True Values" = test$quality)
(169 + 133)/(169 + 133 + 9 + 9)
#.94375


#13. Fit a support vector machine to all of the data using the best cost and gamma parameters you found in part 10.
svm_rad_all = svm(quality ~.,
               data = df,
               kernel = 'radial',
               cost = 1.25,
               gamma = .85
)

#a. How many support vectors does this support vector machine have?
svm_rad_all
#1305

#b. Is the 798th observation a support vector?
svm_rad_all$index
#yes!

#14. What is the overall error rate for the support vector machine you created in part 13?
pred_rad = predict(svm_rad$best.model, df)
table("Predicted Values" = pred_rad, "True Values" = df$quality)

#15. Visualize the support vector machine by examining the free sulfur dioxide and total sulfur dioxide cross-section; to do so, use the following line of code (modified with your object names):  plot(model, data, free.sulfur.dioxide ~ total.sulfur.dioxide). 
plot(svm_rad_all, df, free.sulfur.dioxide ~ total.sulfur.dioxide)

#16. List a pro and con for both:

#a. The best support vector classifier you found in part 7.
#The linear model makes it easier to interpret, but requires that the data be linearly related.

#b. The best support vector machine you found in part 13.
#Makes it harder to interpret, but is a better predictor.
