library(PASWR)
library(VIM)
library(mice)
library(Hmisc)
library(kknn)

data(titanic3)


#1. How many variables contain at least one missing value?
count = 0
for (i in 0:length(titanic3)) {
  if (sum(is.na(titanic3[,i])) > 0) {
    count = count +1
  }
}
print(count)

#a. What are these variables?
for (i in 0:length(titanic3)) {
  if (sum(is.na(titanic3[i])) > 0) {
    print(titanic3[0,][i])
  }
}

#b. For each variable, what is the extent of missingness (how many missing values are there and what is the percentage of missingness)?
for (i in 0:length(titanic3)) {
  if (sum(is.na(titanic3[,i])) > 0) {
    print(titanic3[0,][i]) 
    print(sum(is.na(titanic3[,i])))
  }
}

# D.D. - Can you do these without loops?


#2. How many observations contain at least one missing value?
sum(complete.cases(titanic3))
#a. What is the percentage of missingness from an observation standpoint?
sum(complete.cases(titanic3))/count(titanic3)


#3. How many cells in the data are missing values?
count = 0
for (i in 0:length(titanic3)) {
  if (sum(is.na(titanic3[,i])) > 0) {
    count = count + sum(is.na(titanic3[,i]))
  }
}
print (count)

#a. What is the percentage of missingness from a dataset standpoint?
print(count/(length(titanic3$pclass)*length(titanic3)))


#4. What are the different combinations of missingness in the dataset?
#body and age seem to be the main combination of missingness
md.pairs(titanic3)


#5. What kind of missingness do you have for each variable that contains missing values? Give a reason and scenario as to why you believe this.
#We have missing at random data, as body and age are closely linked. A theory I have is that only some passengers who died receieved body ID numbers (while no passengers who died received them). Since they were dead, they might not be able to identify how old they were?

#6. Impute using mean value imputation for the age variable.
nas_remove = titanic3
nas_remove$age[is.na(nas_remove$age)] = mean(titanic3$age, na.rm=TRUE)

#a. Graph the distributions of the age variable before and after mean value imputation. Describe what you see. What problems may arise?
hist(titanic3$age)
hist(nas_remove$age)
#There is a huge line of observations around 29 years old. Also it looks like mean was skewed by the older people on the ship.

#7. Impute using simple random imputation for the age variable.
nas_remove = titanic3
nas_remove$age = impute(nas_remove$age, "random")

#a. Graph the distributions of the age variable before and after simple random imputation. Describe what you see. What problems may arise?
hist(titanic3$age)
hist(nas_remove$age)
#The graphs look very similar. If the missing data is biased (e.g. if older people were more likely to die, thus leading to more missing values among that age range), then this new dataset will be biased.


#Question #2: K-Nearest Neighbors with the Titanic Dataset

#1. Impute using the single missing value of the fare variable using simple random imputation. What value was imputed?
nas_remove$fare = impute(nas_remove$fare, "random") #Simple random imputation using the
nas_remove$fare[1226]
#29.125

#2. Plot the simple random imputation of fare against the simple random imputation of age; color this plot by pclass. Describe any trends.
ggplot(nas_remove, aes(age, fare)) + geom_point(aes(color = pclass))

#The third class has way more children, and way fewer older passengers. First class has much higher fares, and much higher variance as well.

#3. Add two points to your plot representing the following passengers:
#  a. A 50 year old who paid $400 for their ticket.
#b. A 10 year old whose parents paid $100 for their ticket.
nas_remove = select(nas_remove, fare, age, pclass) #Must uninstall MASS and PASWR for this to work.
nas_remove = rbind(nas_remove, c(400, 50, NA), c(100, 10, NA))

#4. What classes would you think these new individuals would belong to?
#I think they would both belong to the first class.

#5. Impute the missing class values for the new passengers using 1 Nearest Neighbor. What were the predicted classes for each passenger?
imputed.1nn = kNN(nas_remove, k = 1)
tail(imputed.1nn)
#They were predicted to be in the 1st class.

#6. Impute the missing class values for the new passengers using the √n Nearest Neighbor rule. What were the predicted classes for each passenger? Why did they change/not change?
imputed.1nn = kNN(nas_remove, k = 36)
tail(imputed.1nn)

#The 10 year old was predicted to be in the 3rd class now. This was because there were so few children in the first class, but so many children in 3rd class that the 3rd class overwhelmed them. What a metaphor!



#Question #3: Minkowski Distances with the Titanic Dataset

#1. Create a new data frame that includes:
#a. The pclass, survived, sex, age, sibsp, and parch variables from the original titanic3dataset.
new_titanic = select(titanic3, pclass, survived, sex, age, sibsp, parch, fare)

#b. The simple random imputation of the fare variable you created above.
new_titanic$fare = impute(new_titanic$fare, "random")

#2. Separate this new data frame into two separate data frames as follows (note that there should be no observations that appear in both data frames):
#a. For observations that are totally complete: all variables.
titanic_complete = select(titanic3[complete.cases(titanic3),], pclass, survived, sex, sibsp, parch, fare,age)

#b. For observations that are missing a value for age: all variables except age.
titanic_missing_age = select(titanic3[is.na(titanic3$age),], pclass, survived, sex, sibsp, parch, fare)

#3. Use 1 Nearest Neighbor to impute using:
#a. Manhattan distance
new_titanic.manhattan = kknn(age ~ ., titanic_complete, titanic_missing_age, k = 1, distance = 1)


#2b. Euclidean distance
new_titanic.euclidean = kknn(age ~ ., titanic_complete, titanic_missing_age, k = 1, distance = 2)

#c. Minkowski distance with p = 10 
new_titanic.minkowski = kknn(age ~ ., titanic_complete, titanic_missing_age, k = 1, distance = 10)

#4. Overlay and label four separate density curves: one for each of the three 1 Nearest Neighbor imputed age values, and one for the original complete age observations. Describe what you see any why this might be occurring.

man_dist = data.frame(new_titanic.manhattan$fitted.values)
man_dist = mutate(man_dist, distform = 'manhattan')
man_dist = rename(man_dist, age = new_titanic.manhattan.fitted.values)

euc_dist = data.frame(new_titanic.euclidean$fitted.values)
euc_dist = mutate(euc_dist, distform = 'euclidian')
euc_dist = rename(euc_dist, age = new_titanic.euclidean.fitted.values)


mink_dist = data.frame(new_titanic.minkowski$fitted.values)
mink_dist = mutate(mink_dist, distform = 'minkowski')
mink_dist = rename(mink_dist, age = new_titanic.minkowski.fitted.values)

original = select(titanic_complete, age)
original = mutate(original, distform = 'original')
  
all_dist = rbind(man_dist, euc_dist, mink_dist, original)

ggplot(all_dist,aes(age, color = distform))+geom_density()

#We see the estimated ages overwhelmingly concentrated in the 25-35 age range. This is likely due to the fact that more 25-35 y/os died in the titanic than any other age range, thus when k-nearest equalled 1, we get almost all people in that age range.


#5. Use the √n Nearest Neighbor rule to impute using:
#a. Manhattan distance.
new_titanic.manhattan = kknn(age ~ ., titanic_complete, titanic_missing_age, k = 36, distance = 1)

#b. Euclidean distance.
new_titanic.euclidean = kknn(age ~ ., titanic_complete, titanic_missing_age, k = 36, distance = 2)

#c. Minkowski distance with p = 10 .
new_titanic.minkowski = kknn(age ~ ., titanic_complete, titanic_missing_age, k = 36, distance = 10)

#6. Repeat part 4 with the √n Nearest Neighbor solutions. What is happening here?

man_dist = data.frame(new_titanic.manhattan$fitted.values)
man_dist = mutate(man_dist, distform = 'manhattan')
man_dist = rename(man_dist, age = new_titanic.manhattan.fitted.values)

euc_dist = data.frame(new_titanic.euclidean$fitted.values)
euc_dist = mutate(euc_dist, distform = 'euclidian')
euc_dist = rename(euc_dist, age = new_titanic.euclidean.fitted.values)


mink_dist = data.frame(new_titanic.minkowski$fitted.values)
mink_dist = mutate(mink_dist, distform = 'minkowski')
mink_dist = rename(mink_dist, age = new_titanic.minkowski.fitted.values)

original = select(titanic_complete, age)
original = mutate(original, distform = 'original')

all_dist = rbind(man_dist, euc_dist, mink_dist, original)

ggplot(all_dist,aes(age, color = distform))+geom_density()

#We still see the huge spikes in age. This is likely because the data is biased from which we are determining the ages.

# D.D. - Well done, Christian!