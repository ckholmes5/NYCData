#Question #1:
#Use the TimesSquareSignage.csv in the homework folder and import it into R. Then check the following features of the dataset:
df = read.csv('~/Desktop/Work/Bootcamp/Homeworks/Intro to R HW1/TimesSquareSignage.csv')

#1. The number of observations and the number of variables
#2. The type of each variable
str(df)

#3. How many missing values are there in the dataset?
sum(is.na(df))

#4. Which rows (people) have missing value? Which columns (variables) include missing value?
#is.na(df) will give this answer, does that work?

# D.D. - You could use `which` as in the solutions, or df[rowSums(is.na(df))>0,].  

#Question #2:
#From the Time Square dataset, we'd like to extract specific information about advertising in Midtown Manhattan. Obtain the following data frames and save them in CSV files:
#1. Observations from Upper Broadway
broadway = df[df$Location == 'Upper Bway',]
write.csv(broadway, file = '~/Desktop/broadway.csv')

#2. Observations with greater-than-average square footage
above_average_sf = df[df$SF > mean(df$SF),]
write.csv(above_average_sf, file = '~/Desktop/above_average_sf.csv')

#3. The name, address and location of the top observations in terms of total square footage
#assuming top observations means top 10
top10 = head(df[order(-df$TOTAL.SF),c('Screen.Name..LED...Vinyl.Signs.', 'Building.Address', 'Location', 'TOTAL.SF')], 10)
write.csv(top10, file = '~/Desktop/top10.csv')

#Question #3:
#1. From your RStudio,import the built-in data by running data(cars) 
data(cars)

#2. Print the first 5 lines from cars. 
head(cars,5)

#3. Randomly generate a vector as long as the number of rows in cars, and have elements NY, CA or CT. Call the vector state.
state = sample(c('NY', 'CT', 'CA'), length(cars$speed), TRUE)

#4. Add state to the data frame cars as a new column. Again name the column state.
cars = data.frame(cbind(cars, state))
# D.D. - Could also do cars$state <- state to create a new column in cars.  

#5. Create a new column ratio whose value is the ratio dist/speed. Then compute the average and standard deviation
ratio = cars$dist/cars$speed
cars = data.frame(cbind(cars, ratio))
mean(cars$ratio)
sd(cars$ratio)

# D.D. - Well done!