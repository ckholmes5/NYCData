
/* Question #1: */
select sqlexercise;
/*1. Write a query to return columns id and extra from table SLEEP. */
select id, extra from SLEEP;

/*2. Rewrite the previous query so that extra will appear as the first column in your query result.*/
select extra, id from SLEEP;

/*3. Write a query to return all the category values, without repetitions.*/
select * from SLEEP;

/*4. Write a query to return every id whose extra > 0. */
select id, extra from SLEEP where extra > 0;

/*5. Write a query to return the total of extra in each category (call it extraSum) and the number of records in each category (call it categoryNum). */
select category, sum(extra) as sumextra from SLEEP group by category;

/*6. Write a query to return the average extra of each category (call it mean_extra). */
select category, avg(extra) as mean_extra from SLEEP group by category;

/*Question #2: */

/*1. Select the first two rows in table Department. */
select * from Department limit 2;

/*2. Write a query to return the employee name, hire date, basewage from table Employee. */
select EmployeeName, HireDate, BaseWage from Employee;

/*3. Write a query to return the total wage of employees. total wage = basewage * baselevel */
select BaseWage*WageLevel as TotalWage from Employee;

/*4. Write a query to return names of employees whose basewage ranges from 2000 to 3000,sort the result by basewage in descending order. */
select EmployeeName,BaseWage from Employee where BaseWage >= 2000 & BaseWage <= 3000 order by BaseWage DESC;

/*5. Write a query to return the employeename, hiredate, basewage whose name ends with 8 and who was hired after June 10, 2010. (Hint: read pattern matching in Mysql) */
select EmployeeName, HireDate, BaseWage from Employee where EmployeeName 

select EmployeeName, HireDate, BaseWage from Employee where EmployeeName like '%8' whereselect EmployeeName, HireDate, BaseWage from Employee where EmployeeName like '%8' AND DATE(HireDate) > DATE('2010-06-01');

/*6. Write a query to return the employeename and corresponding departmentid whose total wage is larger than 7000. */
select EmployeeName,DepartmentID from Employee where BaseWage * WageLevel >= 7000;

/*7. Write a query to return the department id of departments that have at least 2 employees with basewage >= 3000. */
select DepartmentID from (select DepartmentId, count(BaseWage) from Employee where BaseWage >= 3000 group by DepartmentID having count(BaseWage) >= 2) as temp;
/* D.D. - Much more succinct is, e.g., 
select DepartmentID from Employee where BaseWage>=3000 group by DepartmentID having count(*)>=2; 
*/

/*8. Write a query to return the average total wage in each department. Sort the results by average wage in ascending order. */
select DepartmentID, avg(BaseWage * basewage) from Employee group by DepartmentID order by avg(BaseWage) ASC;

/*9. Write a query to return the average total wage of males and females in each department. Sort the results by Department ID in descending order. */
select EmployeeSex, DepartmentID, avg(BaseWage * WageLevel) as TotalWage from Employee group by DepartmentID, EmployeeSex order by DepartmentID DESC; 

/*10. Write a query to return the name of each employee, along with his/her deparmentname and the principal in the department. (Hint: use JOIN.) */
select * from Employee join Department on Employee.DepartmentID = Department.DepartmentID;

/*Question #3: */
/*1. Download data set; and upload it to the server using scp. Create a table named adult which has the same structure.*/
CREATE TABLE adult (age integer, workclass char(100), fnlwgt integer, education char(100), education_num integer, marital_status char(100),occupation char(100), relationship char(100), race char(50), sex char(10), capital_gain integer, capital_loss integer, hours_per_week integer, native_country char(100), class char(100));

/*2. Load the data set a dult.data into table adult */
load data local infile 'adult.data' into table adult fields terminated by ' ,' optionally enclosed by '"' lines terminated by '\n';

/*3. Are there any missing values in the table? How many rows have missing values? a. For numerical fields, use the ‘is null’ condition. */
select count(*) from adult where  (age is null or workclass is null or education_num is null or capital_gain is null or capital_loss is null or hours_per_week is null);

/*b. For string fields, missing values are represented as “?”. */
SELECT count(*) from adult where  (fnlwgt="?" or education="?" or marital_status="?" or occupation="?" or relationship="?" or race="?" or sex="?" or native_country="?" or class="?");

/*4. Remove the rows having missing values. */
delete from adult where (age is null or workclass is null or fnlwgt="?" or education="?" or education_num is null or marital_status="?" or occupation="?" or relationship="?" or race="?" or sex="?" or capital_gain is null or capital_loss is null or hours_per_week is null or native_country="?" or class="?");

/*5. What's the ratio of number of '<=50K' / number of '>50K' in column class. */
select poor/rich as ratio from (select count(class) as poor from adult where class ='<=50K') as poor, (select count(class) as rich from adult where class ='>50K') as rich;


/*6. Compute the average age in each class. */
select avg(age), class from adult group by class;

/*7. How many rows in class ‘>50K' where the age is less than 36.78? */
select count(*) from adult where class = '>50K' and age < 36.78;

/*8. What's the average hours-per-week in each class? */
select class, avg(hours_per_week) as avg_hours from adult group by class;

/*9. What's the ratio of number of '<=50K'/numberof'>50K' in FemaleandMale (column sex)? */
select poor/rich as ratio from (select count(class) as poor, sex from adult where class ='<=50K') as poor, (select count(class) as rich, sex from adult where class ='>50K') as rich group by sex;

-- D.D. - Well done!
