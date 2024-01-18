# Overview
Keys are used to uniquely identify and reference records 

# Types
## Primary
- Must be unique
- Uniquely identifies a record
- [[Data Integrity#Key|Can't be null]]

## Foreign
- A reference to a primary key in another table
- Used for creating relationships between records

## Composite
- A primary key made up of multiple fields that don't uniquely identify the field on their own
- Often used in tables that have multiple foreign keys

## Compound
- A composite key made up of only foreign keys

# Superkeys
- A method for finding the fewest attributes required to make a primary key

> [!NOTE] Superkeys are unique
> If two tuples are not the same, their superkeys will be different
> 
> $$
> t_{1} \ne t_{2} \rightarrow t_{1}[SK] \ne t_{2}[SK]
> $$
> for any 2 tuples $t_{1}$ and $t_{2}$ in a relation $R$

## Process
1. Take the [[Maths Map#Set theory|set]] of attributes and create combinations that could uniquely identify a record
2. Find the [[subsets]] with the lowest [[Set Operations#Cardinality|cardinality]]. These are the [[#candidate keys]]
3. Pick the candidate key that makes the most practical sense for the table

$$
\begin{align*}
&TABLE\ Employee(SSN, Ename, Lname, Bdate, Salary, Dno)\\

&- \{SSN, Ename, Bdate\}\\
&- \{SSN\}\\
&- \{SSN, Ename\}\\
&- \{Ename, Salary\}
\end{align*}
$$
$\{SSN\}$ is the candidate key

> [!NOTE] Formal process
> $$
> K \textrm{is candidate key} \leftrightarrow K' = K \\ \{A_{i}\} \textrm{ is not a } SK \textrm{ for any } A_{i} \in K
$$

## Candidate keys
- There may be more than one candidate key
- The choice of primary key is sometimes arbitrary
- The remaining candidate keys are called secondary keys