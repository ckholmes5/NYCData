library(HSAUR)
library(psych)

#Question #1: Principal Component Analysis

#Load the  heptathlon dataset from the  HSAUR library into your workspace. A heptathlon is a combined track and field event-based contest for women. This dataset contains scores on each event for the 1988 olympic heptathlon competition held in Seoul.
data("heptathlon")
#1. Create a scatterplot matrix of all variables in the dataset. Briefly comment on the nature of the data.
plot(heptathlon)

#A lot of the variables seem extremely correlated, especially score and long jump.

#2. It will help to have all event scores going in the “same direction” (i.e., a higher event score implies a better performance, and a lower event score implies a worse performance). To do so, transform the hurdle and running variables by subtracting the original scores for each heptathlete from the maximum score of each of those variables, respectively.
heptathlon = mutate(heptathlon, hurdles_transform = abs(hurdles - max(hurdles)), run800m_transform = abs(run800m - max(run800m)), run200m_transform = abs(run200m - max(run200m)))
heptathlon$hurdles <- NULL
heptathlon$run800m <- NULL
heptathlon$run200m <- NULL

#3. Create a scatterplot matrix of all the event score variables in the “same direction.” Briefly comment on the nature of the data.
plot(heptathlon)
#All the variables now appear to be positively correlated with score. Mission accomplished!

#4. Create a scree-plot of your newly created dataset that doesn’t include the score variable. From this plot, describe how you determine the number of principal components to extract in three different ways.
fa.parallel(heptathlon, fa = "pc", n.iter = 100)
abline(h=1)

#1. "Elbow method": Select up to and including the component where there is a significant bend in the graph (i.e. the elbow).
#2. Above average method: Select only the components that contribute an above average amount of the components to the data.
#3. You could also run simulations and extract eigenvalues from random data matrices of the same dimension as your data; find where the parallel analysis overshadows real data.

#5. Extract the appropriate number of principal components from your dataset that does not include the score variable and save this object.
pc_hept = principal(heptathlon, nfactors = 2, rotate = "none")
pc_hept

#6. What is the variance of the each of your extracted principal components?
#PC1 = 4.74
#PC2 = 1.14

#7. How much variability in the original dataset is captured by each of your extracted principal components?
#PC1 = 0.68
#PC2 = 0.15

#8. Create a plot of the principal component loadings against each other.
factor.plot(pc_hept, labels = colnames(heptathlon))

#9. Use the object you created in part 5 and the plot in part 8 to help construct interpretations for each principal component vector.
#It looks like most of the variance in the dataset can be explained by PC1, outside of the column javelin. We need a second component to explain the variance in the javelin (as it appears to be not very correlated with the rest of the variables)

#10. Create a scatterplot of each of the competitor’s results projected onto the reduced dimensions.
plot(heptathlon) #original dimensions
plot(pc_hept$scores) #reduced dimensions

#11.Comment on any observations that appear to be outliers. Who are these competitors and why do they appear to be outliers?
pc_hept$scores
heptathlon
#It appears that there is one competitor who finished dead last almost every event, including nearly 20 seconds behind everyone else in the 800m. 


