---
tags: SQL
---
# Overview
Joins match tuples from two different relations on an attribute

## Motivation
Selecting from two tables actually returns the [[Set Operations#Cartesian product|cross product]], leading to fictitious tuples

# Types

![Types of joins](https://miro.medium.com/v2/resize:fit:1400/0*rFMChX4SAmQ9RzF9.png)

Here is an interactive example: https://joins.spathon.com/

## Inner
AKA theta-join or equi-join

- Matches tuples on a [[Keys#Primary|primary key]] and [[Keys#Foreign|foreign key]]
- Returns tuples if there is a matching tuple
	- Won't return a tuple if the foreign key is `NULL`

```SQL
SELECT Fname,
		Lname, 
		Address
FROM (EMPLOYEE JOIN DEPARTMENT
				ON Dno = Dnumber)
WHERE Dname = "Research";
```

You can use the `WHERE` clause to do this as well

```sql
SELECT Fname,
		Lname, 
		Address
FROM EMPLOYEE,
	DEPARTMENT
WHERE Dname = "Research"
	AND EMPLOYEE.Dno = DEPARTMENT.Dnumber;
```

## Outer
- Matches tuples on a key 
- Includes tuples where there isn't a match

```sql
SELECT Customers.CustomerName,
	Orders.OrderID  
FROM Customers  
FULL OUTER JOIN Orders
	ON Customers.CustomerID=Orders.CustomerID  
ORDER BY Customers.CustomerName;
```

## Left outer
- All tuples in the left relation are returned
- If there isn't a matching tuple in the right relation, its attributes are filled with `NULL`

```sql
SELECT Customers.CustomerName,
	Orders.OrderID  
FROM Customers  
LEFT JOIN Orders
	ON Customers.CustomerID = Orders.CustomerID  
```

## Right outer
- All tuples in the right relation are returned
- If there isn't a matching tuple in the left relation, its attributes are filled with `NULL`

```sql
SELECT Orders.OrderID,
	Employees.LastName,
	Employees.FirstName  
FROM Orders  
RIGHT JOIN Employees
	ON Orders.EmployeeID = Employees.EmployeeID  
```
