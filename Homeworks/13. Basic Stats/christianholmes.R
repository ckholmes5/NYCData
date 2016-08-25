#Foundations of Statistics
#Question #1: Body Temperature
#The  [01] Temp.txtdataset records the body temperatures, gender, and heart rate of 130 individuals; you may assume that the observations are independent of one another.
#1. Read the data into R and provide basic numerical EDA to describe the data.
stats = read.csv('~/Desktop/Work/Bootcamp/Homeworks/FoundationsofStatistics/[01] Temp.txt', sep = ' ')
summary(stats)



#a. Provide univariate EDA for all variables, and bivariate EDA where appropriate.
summary(stats$Body.Temp)
summary(stats$Heart.Rate)
summarise(group_by(stats, Gender), mean(Body.Temp), median(Body.Temp), mean(Heart.Rate), median(Heart.Rate))
males = filter(stats, Gender == 'Male')
females = filter(stats, Gender == 'Female')

#2. Provide basic graphical EDA to describe the data.
#a. Provide univariate EDA for all continuous variables, and bivariate EDA where appropriate.
boxplot(stats$Body.Temp)
boxplot(stats$Heart.Rate)
boxplot(females$Body.Temp)
boxplot(males$Body.Temp)
boxplot(females$Heart.Rate)
boxplot(males$Heart.Rate)

#3. You have heard that the average human body temperature is 98.6 degrees Fahrenheit. Does this data support this claim? (Perform a hypothesis test to determine whether the true population mean body temperature is 98.6 degrees Fahrenheit.)
t.test(stats$Body.Temp, mu = 98.6)
#The data does not support this finding, the p-value indicates it's extremely unlikely.

#a. What is a 95% confidence interval for the average human body temperature?
#98.12200-98.37646

#b. Interpret your results in context of the problem.
#This suggests the mean of the population from which this sample was taken will fall between 98.122 degrees and 98.376 degrees 95% of the time.


#4. Is there a significant difference in body temperature between males and females? If so, quantify this difference.
t.test(males$Body.Temp, females$Body.Temp)
#The mean body temperature for women is 98.1, while the mean for men is 98.4. This difference is statistically different at the .05 level.

#a. What is a 95% confidence interval for the average difference in human body temperature between males and females?
#0.03881298 to 0.53964856 degrees 

#b. Interpret your results in context of the problem.
#This suggests that the true difference between men and women is between .03 and .54 degrees.

#5. You believe the variances of male heart rate and female heart rate are different; specifically, you claim that females have a lower heart rate variance. Test this claim.
var.test(females$Heart.Rate, males$Heart.Rate)

#a. What is a 95% confidence interval for the ratio between female and male heart rate variances?
#1.16086 3.12029

#b. Interpret your results in context of the problem.
#This suggests that females have a higher variance in heart rates than males do.


#Question #2: Plant Growth
#Load the  PlantGrowth dataset located in the  datasets library; this dataset contains dried plant weight measurements for the same species of plant under three different conditions (two separate growth treatments, and a control group where no treatment was applied). You may assume that the observations are independent of one another.
plants = data("PlantGrowth")

#1. Create side-by-side boxplots of the plant weights segmented by the type of applied treatment. Describe the features of the graph.
ggplot(data = PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
#Treatment 1 has much higher weights than both treatment 2 and the control. 

#2. Calculate the standard deviations of each conditional distribution of plant weight based on the applied treatment. Do these differ significantly?
summarise(group_by(PlantGrowth, group), sd(weight))

#3. NB: To avoid increasing our chance of encountering a “false positive,” we must avoid applying three separate F-tests (treatment #1 vs treatment #2; treatment #1 vs control; treatment #2 vs control). As an alternative, Bartlett’s Test of Homogeneity of Variances allows us to simultaneously test for the similarity of a group of variances, rather than just a pair. Implement this test using the bartlett.test() function and report your results.
bartlett.test(PlantGrowth$weight, PlantGrowth$group)

#3. Is there a significant difference in the weight of plants based on the growth treatments they were given? Conduct a hypothesis test and report your results in context of the problem.
trt1 = filter(PlantGrowth, group == 'trt1')
trt2 = filter(PlantGrowth, group == 'trt2')
t.test(trt1$weight, trt2$weight)
#There is a significant difference between treatment types, even though n=10 for each. 

#a. Given the results of the Bartlett test, is the result of your hypothesis test valid?
#Yes, there is not conclusive evidence that the variances are different between the populations.

# D.D. - I think something went wrong here...  First, you should use one-way ANOVA because
# there are > 2 groups.  Second, the p-value of the Bartlett test should be insignificant, implying
# that there is no statistically significant difference in variances between the three groups, thus
# defending the use of the one-way ANOVA.  




#Question #3: Gender, Hair, & Eye Color
#Load the HairEyeColor dataset located in the  datasets library; this is a three dimensional dataset that records the hair color, eye color, and gender of 592 different statistics students.
data("HairEyeColor")

#1. Visualize the entire dataset using a mosaic plot using the following command: a. mosaicplot(HairEyeColor,shade=TRUE)
mosaicplot(HairEyeColor,shade=TRUE)

#1. Visualize the entire dataset using a mosaic plot using the following command: 
#  a. mosaicplot(HairEyeColor,shade=TRUE)
mosaicplot(HairEyeColor,shade=TRUE)
      
#  b. Which category combinations receive more observations than expected? Fewer observations than expected?
  #Blond females with brown eyes were lower than expected, blond females with blue eyes were higher than expected. 
  

#2. Reduce your dataset to focus on just females with brown and blue eyes (but still include all hair colors). Create another mosaic plot and describe what you see.
  reduced = HairEyeColor[ c('Brown', 'Blond') , , 'Female' ]
  mosaicplot(reduced,shade=TRUE)
  
#a. Conduct a hypothesis test to see if hair and eye color are independent of one another for this reduced dataset. Report your results in context of the problem.
  chisq.test(reduced)
#p-value = 2.57e-15, so there is absolutely a difference.
  
# D.D. - Not "a difference", but evidence to reject H_0 (that hair and eye color are independent)
# in favor of H_a (that hair and eye color are dependent).  
  
#3. For the reduced dataset, which category combination contributed most to any statistical deviations? Which category contributed the least? By how much for each?
#Blond hair, brown eyes contributed the most, while brown hair brown eyes contributed the least.
  
# D.D. - ? 




