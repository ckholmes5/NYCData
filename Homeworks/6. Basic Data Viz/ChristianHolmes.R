library(ggplot2)
library(dplyr)

#Question 1
df = read.csv('~/Desktop/Work/Bootcamp/Homeworks/ggplot HW/Champions.csv')

#Use filter to find the rows (games) that home team wins. Also use filter to find out rows that the HomeTeam is either "Barcelona" or "Real Madrid".
tbl_df = filter(df, HomeGoal > AwayGoal)
filter(df, HomeTeam == 'Barcelona' | HomeTeam == 'Real Madrid')

#Use select to create a new table which exactly includes all the variables about home team (and excludes variables about away team). Create another table which only includes 6 columns: HomeTeam, AwayTeam, HomeGoal, AwayGoal, HomeCorner, and AwayCorner. Hint: you may use the argument starts_with or contains  in the function select . 
hometeamvars = select(df, starts_with(Home))
six_df = select(df, HomeTeam, AwayTeam, HomeGoal, AwayGoal, HomeCorner, AwayCorner)

#Use arrange to reorder the dataset by the number home goals, and display the following 6 columns of the reordered data: HomeTeam, AwayTeam, HomeGoal, AwayGoal, HomeCorner, and AwayCorner.
six_df = arrange(six_df, desc(HomeGoal)) #they're already in the correct order but if you want to rearrange:
six_df = select(six_df, HomeTeam, AwayTeam, HomeGoal, AwayGoal, HomeCorner, AwayCorner)

#For each HomeTeam, find out its average HomeGoal, average HomePossession (possession rate), and average HomeYellow (number of yellow cards). Summarise the results in a table.
summarise(group_by(df, HomeTeam), mean(HomeGoal), mean(HomePossession), mean(HomeYellow))


#Question 2
data(cars)

#Create a scatterplot of dist (y-axis) vs. speed (x-axis).
car_plot = ggplot(cars, aes(speed, dist)) + geom_point()

#Refine the basic plot by labeling the x-axis with "Speed (mpg)" and the y-axis with "Stopping Distance (ft)". Also add a title to the plot.
car_plot + labs(title = 'Stopping Distance vs. Speed', x = 'Speed (mpg)', y = "Stopping Distance (ft)")


#Revise the plot by changing the every point from the default open circles to red filled triangles (col="red", pch=17).
ggplot(cars, aes(speed, dist)) + geom_point(colour = 'red', shape = 17)


#Question 3
#Display the Beta(2, 6), Beta(4, 4), and Beta(6, 2) densities on a same plot. (Hint: specify the argument add=TRUE in the curve function.)
plot.new()
plot.window(xlim=c(0,1), ylim=c(-3,3))
curve(dbeta(x, 5, 2), from=0, to=1)
curve(dbeta(x, 2, 6), from=0, to=1, add = TRUE, col = 'red')
curve(dbeta(x, 4, 4), from=0, to=1, add = TRUE, col = 'yellow')
curve(dbeta(x, 6, 2), from=0, to=1, add = TRUE, col = 'blue')
title(expression(f(y)==frac(1,B(a,b))*y^{a-1}*(1-y)^{b-1}))

#Label each density curve with its corresponding shape parameters a and b using text function.
text(x=0.2,y=dbeta(0.2,2,6),labels=c('(x,y)=(2,6)'),pos=2)
text(x=0.2,y=dbeta(0.2,2,6),labels=c('(x,y)=(4,4)'),pos=2)
text(x=0.2,y=dbeta(0.2,2,6),labels=c('(x,y)=(6,2)'),pos=2)





#Question 4
data(faithful)


#In the faithful data frame, add a variable length that is "short" if the eruption is less than 3.2 minutes, and "long" otherwise.
faithful = mutate(faithful, length = ifelse(eruptions < 3.2, 'short', 'long'))

#Create parallel boxplots of the waiting times for the “short” and “long” eruptions.
ggplot(faithful, aes(length, waiting)) + geom_boxplot()

#Create overlapping density curves of the waiting times of the “short” and “long” eruptions.
ggplot(faithful, aes(waiting)) + geom_density(aes(color =length))

#Briefly describe your findings from the boxplots and the density curves.
#For long geyser eruptions, you generally have to wait longer, with the median waiting time right around 80 seconds. By comparison, the waiting time for shorter eruptions is roughly 30 seconds shorter, clocking in at 54 seconds.


#Question 5

#Calculate the winning ratio of New York Knicks in different Seasons. Visualize how the winning ratio changes every year. (Barplot is the most appropriate here.)
load("/Users/cholmes/Desktop/Work/Bootcamp/Homeworks/ggplot HW/Knicks.rda")
knicks = data

#Calculate the winning ratio of New York Knicks in different Seasons. Visualize how the winning ratio changes every year. (Barplot is the most appropriate here.
knicks = mutate(knicks, win_num = ifelse(win == 'W', 1, 0))
grouped_knicks = summarise(group_by(knicks, season), wins = sum(win_num), n = n())
grouped_knicks = mutate(grouped_knicks, win_ratio = wins/n)

ggplot(data = knicks, aes(season)) + geom_bar(aes(fill = win))


#Calculate the winning ratio both home and away. (The row labelled with v isiting = 1 is an away game.) Create bar-plots to show home and away winning ratios for each season.
grouped_knicks = knicks %>% 
  group_by(season,visiting) %>%
  summarise(Total=n(), Wins=sum(win=="W"), ratio=Wins/Total)
grouped_knicks

ggplot(knicks, aes(season)) + geom_bar(aes(fill = wins))

#Plot five histograms to display the distribution of points each season.
ggplot(knicks,aes(points)) + geom_histogram(binwidth=5) + facet_wrap(~season)


# D.D. - Excellent job, Christian!







