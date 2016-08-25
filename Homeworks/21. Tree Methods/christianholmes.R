#Question #1: Trees
#Load the OJ dataset from the ISLR library into your workspace. The data contains 1,070 purchases where the customer either purchased Citrus Hill or Minute Maid orange juice. A number of characteristics of the customer and product are recorded.
library(ISLR)
library(tree)
library(randomForest)
library(gbm)

data(OJ)

#1. Split the data into a training and test set with an 80% - 20% split, respectively.
set.seed(0)
train1 = sample(1:nrow(OJ), 8 * nrow(OJ)/10)
train = OJ[train1,]
OJ.test = OJ[-train1, ]

#2. Construct an initial decision tree predicting Purchase from all other variables in the training dataset defining splits based upon the Gini coefficient.
tree.oj = tree(Purchase ~ ., data = train, split = 'gini')

#3. How many terminal nodes are there in your initial tree? What is the accuracy of your initial tree?
summary(tree.oj)
#86 Terminal Nodes in the initial tree
#We got 732/856, for an accuracy rate of 85.51%

#4. Predict the Purchase variable for observations that are within your test set using this initial tree. Report the accuracy of your predictions.
oj.predict = predict(tree.oj, test, type = 'class')
table(oj.predict, test$Purchase)
accuracy = (106 + 59)/(106 + 24 + 25 + 59)
accuracy
#77.1% accuracy, not bad for the first try!

#5. Implement cross-validation and, thus, cost-complexity pruning to determine how far back to prune your tree.
set.seed(0)
cv.oj = cv.tree(tree.oj)

#6. Visualize your results from part 5 across various numbers of terminal nodes/values for alpha.
par(mfrow = c(1, 2))
plot(cv.oj$size, cv.oj$dev, type = "b",
     xlab = "Terminal Nodes", ylab = "RSS")
plot(cv.oj$k, cv.oj$dev, type  = "b",
     xlab = "Alpha", ylab = "RSS")

#7. Prune your tree based on the results of part 6.
prune.oj = prune.tree(tree.oj, best = 17)
par(mfrow = c(1, 1))
plot(prune.oj)

#8. How many terminal nodes are there in your pruned tree? What is the accuracy of your pruned tree?
summary(prune.oj)
#22 Terminal nodes this time, with an accuracy of 82.71%

#9. Visualize your pruned tree.
plot(prune.oj)

#10. Predict the Purchase variable for observations that are within your test set using this pruned tree. Report the accuracy of your predictions.
oj.predict = predict(prune.oj, test, type = 'class')
table(oj.predict, test$Purchase)
accuracy = (108 + 63)/(108 + 63 + 20 + 23)
#Accuracy of 79.9%

#11. Why are the test set predictions more accurate for the pruned tree than those for the initial tree?
#We're now predicting with the optimal number of nodes, and thus we aren't overfitting our model.

#Question #2: Bagging & Random Forests
#Continue using the OJ dataset and the training/test sets you already loaded into your workspace.
#1. Construct an initial random forest predicting Purchase from all other variables in the training dataset using the default settings; this will create 500 trees.
set.seed(0)
rf.oj = randomForest(Purchase ~ ., data = train)

#2. What is the accuracy of this initial random forest on:
#a. The training set?
rf.oj
(446 + 244)/ (446 + 244+ 90 + 76)
#Accuracy = 80.6%

#b. The test set?
oj.predict = predict(rf.oj, test, type = 'class')
table(oj.predict, test$Purchase)
(113 + 60)/(113 + 60 + 23 + 18)
#Accuracy = 80.8%

#3. Which variable is aiding the most in classifying the orange juice purchases?
varImpPlot(rf.oj)
#LoyalCH was by far the most important in classifying orange juice purchases

#4. Vary the number of variables considered as candidates at each node split in the random forest procedure (from one to all predictors). Record the out-of-bag error rates for each of these random forests on the training set. (Hint: You will want to record the error rate instead of the MSE since this is a classification problem. If you are modifying class code, try using the code snippet fit$err.rate[500, 1].
set.seed(0)
oob.err = numeric(18)
for (mtry in 1:17) {
  fit = randomForest(Purchase ~ ., data = train, mtry = mtry)
  oob.err[mtry] = fit$err.rate[500, 1]
  cat("We're performing iteration", mtry, "\n")
}

#5. Visualize the out-of-bag error rates as they change with the number of variables considered at each node split.
plot(1:18, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")

#6. What is the maximum accuracy among your random forests on the training set? How many variables were considered at each split in this best random forest?
1 - min(oob.err[1:17])
oob.err == min(oob.err[1:17])
#Maximum accuracy was 81.3%, which occured in the 3 variable model

#7. What is the accuracy of the bagged model on the training set? How many variables were considered at each split in this bagged model?
bagged =randomForest(Purchase ~., data=train, mtry=17, ntree=500,importance=T)
bagged
(435+246)/(435 + 246 + 88 + 87) 
#79.6%

#8. What is the accuracy of the best random forest from part 6 on the test set?
set.seed(0)
OJ.test = randomForest(Purchase ~ ., data = train , mtry = 2)
OJ.new=predict(OJ.test,test)
table(OJ.new,test$Purchase)
(118+58)/(118+58 + 13 + 25)
#82.2%

#9. What is the accuracy of the bagged model on the test set?
set.seed(0)
OJ.bag=randomForest(Purchase ~.,data=test,mtry=2)
OJ.predict=predict(OJ.bag,test)
table(OJ.predict,test$Purchase)
(111+62)/(111+62 + 20 + 21)
#80.8%

#Question #3: Boosting
#Continue using the OJ dataset and the training/test sets you already loaded into your workspace.
#1. In order to boost with classification trees, we need to do a bit of data munging to transform the response variable. You may use the following lines of code to produce the copies of your dataset OJ.train.indicator and OJ.test.indicator that have a transformed response variable. (NB: You must replace OJ.trainand OJ.testwith whatever names you used in your own code.)
OJ.train.indicator = train 
OJ.test.indicator = test
OJ.train.indicator$Purchase = as.vector(train$Purchase, mode = "numeric") - 1
OJ.test.indicator$Purchase = as.vector(test$Purchase, mode = "numeric") - 1

#2. Construct an initial boosted model on the training set that uses all of the following settings at once:
#a. The Bernoulli distribution.
#b. 10,000 trees.
#c. An interaction depth of 4.
#d. A shrinkage parameter of 0.001.
set.seed(0)
OJ.boost = gbm(Purchase ~ ., data = OJ.train.indicator,
               distribution = 'bernoulli',
               n.trees = 10000,
               interaction.depth = 4,
               shrinkage = 0.001)
summary(OJ.boost)
#3. Predict your test set observations using the initial boosted model across up to 10,000 trees, considering groups of 100 trees at a time. (Hint: Use type = "response")and round your ultimate predictions.)
n.trees = seq(from = 100, to = 10000, by = 100)
predictions = round(predict(OJ.boost, newdata = OJ.test.indicator, n.trees = n.trees,type='response'))
OJ.boost.match = predictions==OJ.test.indicator$Purchase

#4. Calculate and store the accuracy for each of the 100 models considered in part 3.
accuracy = NULL
for (i in 1:100){
  accuracy[i]=sum(OJ.boost.match[,i])/214
}
plot(accuracy)
accuracy[10:40]

#What is the minimum number of trees required to reach the maximum accuracy?
#Highest accuracy is 0.8364486, achieved at 2100 trees.

#5. Plot the accuracies found in part 4 against the number of trees. Add to the plot:
#  a. A horizontal line marking the best boosted accuracy on the test set.
#b. A horizontal line marking the best random forest accuracy on the test set.
#c. A horizontal line marking the best pruned decision tree accuracy on the test set.
plot(accuracy, type='b')
abline(h=max(accuracy))
abline(h=.8224)
abline(h=((114+57)/214))

