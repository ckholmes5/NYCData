library(dplyr)
library(ggplot2)
library(magrittr)

#Question1
#Load ggplot2 package first and then type data(mpg). Quickly go through the dataset and the help file.
data(mpg)

#Obtain a subset of data including: year, cyl, cty, hwy, and renames these variables as V1, V2, V3, V4.
select(mpg, V1 = year, V2 = cyl, V3 = cty, V4 = hwy)

#In mpg data, obtain the average of city miles per gallon and highway miles per gallon for different numbers of cylinders. (Hint: the mean function calculates the average of a vector.)
mean_cty = summarise(group_by(mpg, cyl), mean(cty))
mean_hwy = summarise(group_by(mpg, cyl), mean(hwy))

#For manufacturer, identify the car(s) that have the largest city miles per gallon.
grouped_mpg = select(group_by(mpg, manufacturer), cty, model, manufacturer)
filtered_mpg = filter(grouped_mpg, cty == max(cty))
filtered_mpg = unique(filtered_mpg)

#QUESTION 2
#Create a new variable ratioHVE showing the ratio between highway miles per gallon and engine displacement.
mpg = mutate(mpg,ratioHVE = hwy/displ)

#Create new variables rationCVE showing the ratio between city miles per gallon and engine displacement.
mpg = mutate(mpg, ratioCVE = cty/displ)

#Obtain the average ratioHVE and ratioCVE by different years and manufacturers.
summarise(group_by(mpg, manufacturer, year), mean(ratioHVE), mean(ratioCVE))

#Find the biggest ratioHVE by different years and drv.
summarise(group_by(mpg, drv, year), max(ratioHVE), manufacturer)



#QUESTION 3
#What are the mean and median beginning and ending salaries for each agency? Note that salaries can be annual, hourly, or daily. You need to convert all of them to annual.
salary_data = read.csv('/Users/cholmes/Desktop/Work/Bootcamp/Homeworks/DPYLR HW/NYC_Jobs.csv')

new_sal = select(salary_data,Salary.Range.From, Salary.Range.To, Salary.Frequency, Agency, Posting.Type, Level, X..Of.Positions)
new_sal = mutate(new_sal, Salary.Total.From = (ifelse(Salary.Frequency == 'Annual', Salary.Range.From, ifelse(Salary.Frequency == 'Hourly', Salary.Range.From*262*7, Salary.Range.From*262 ))), Salary.Total.To = (ifelse(Salary.Frequency == 'Annual', Salary.Range.To, ifelse(Salary.Frequency == 'Hourly', Salary.Range.To*262*7, Salary.Range.To*262 ))))
new_sal = mutate(new_sal, average_salary = Salary.Range.To/Salary.Range.From)

sal_by_agency = summarise(group_by(new_sal, Agency), mean(Salary.Range.From), mean(Salary.Range.To), median(Salary.Range.From), median(Salary.Range.To))


#Which agency has the highest average starting salary?
sal_by_agency = rename(sal_by_agency, starting_salary = `mean(Salary.Range.From)`, ending_salary = `mean(Salary.Range.To)`)
filter(sal_by_agency, starting_salary == max(starting_salary))
summarise(sal_by_agency, max(starting_salary))

#Does the type of posting (internal or external) have a big impact on the average salary range?
new_sal = mutate(new_sal, salary_range = Salary.Range.To - Salary.Range.From)
summarise(group_by(new_sal, Posting.Type), mean(salary_range))

#Rank the levels according to the average salary range for each level.
arrange(summarise(group_by(new_sal, Level), mean(salary_range)), desc(`mean(salary_range)`))

#Find out the range of money currently being spent by each agency for new hires. Note that the X..Of.Positions column shows how many positions are available.
new_sal = mutate(new_sal, from_money = Salary.Range.From*X..Of.Positions, to_money = Salary.Range.To*X..Of.Positions)
new_sal = summarise(group_by(new_sal, Agency),  total_from_money = sum(from_money), total_to_money = sum(to_money))

#What civil service title has the largest salary range?
arrange(new_sal, desc(total_to_money))

#not sure if you're looking for an ordered list or one answer, so here's another option:
summarise(new_sal, max(total_to_money))
# D.D. - This question is a little vague.  Probably the most reasonable interpretation
# is that it is asking for the agency with the largest average salary range.  Well done
# on the homework though!

