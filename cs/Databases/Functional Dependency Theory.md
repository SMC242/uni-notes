# Overview
Functional dependency theory aims to quantify the quality of a relational model. There are a few metrics that it aims to express:
- Quality of relations between attributes ([[Keys#Primary|primary keys]], [[Keys#Foreign|foreign keys]])
- How good a model is
- A model's pitfalls
- Efficiency of [[fundamental operations]]
- Minimise repetition of data while storing as much information as possible

Functional dependency is a metric that represents how good a relational schema is

# Definition
- $FD$ is a constraint derived from the relationship between attributes
- If value $Y$ can always be predicted based on the value of $X$, $X$ determines $Y$

> [!EXAMPLE]
> `EMP_PROJ(SSN (PK), Pnumber (PK), Hours, Ename, Pname, Plocation)`
>
>- $SSN \rightarrow Ename$ 
>	- The `Ename` can be looked up with the tuple's `SSN`
>- $Pnumber \rightarrow \{Pname, Plocation\}$
>	- The `Pname` and `Plocation` can be looked up with its `Pnumber`
>- $\{SSN, Pnumber\} \rightarrow Hours$
>	- The `Hours` can be determined by the `SSN` and `Pnumber`

## Notation
- $FD: X \rightarrow Y$  means $X$ uniquely determines $Y$

## Formal definition
- $X \rightarrow Y$ holds if $t_{1}[X] = t_{2}[X] \Rightarrow t_{1}[Y] = t_{2}[Y]$
	- If the $X$ attribute is equal in $t_{1}, t_{2}$, the $Y$ attributes of $t_{1}, t_{2}$ will also be equal
	- $X \rightarrow Y \ in \ R$ is a constraint on all instances

# Inference rules
## Candidate keys
> If $K$ is a [[Keys#Superkeys|candidate key]] for a relation $R$, $K \rightarrow \{R\}$ 

 - $K$ functionally determines all attributes in $R$

## Reflexive
> If $Y \subseteq X$, $X \rightarrow Y$

> [!EXAMPLE]
> $X = \{SSN, Ename\}; X \rightarrow \{SSN\}, X \rightarrow \{Ename\}$

## Augmentation
AKA partial dependency

> If $X \rightarrow Y$, $X \cup \{Z\} \rightarrow Y \cup \{Z\}$

- If you add more attributes to both sides of the dependency, the dependency still holds

> [!EXAMPLE]
> $R(ABCD)$
> $if\ A \rightarrow B \ then \ AC \rightarrow BC$

## Transitive
> If $X \rightarrow Y \land Y \rightarrow Z$, $X \rightarrow Z$

## Decomposition
AKA project

> If $X \rightarrow \{Y, Z\}$, $X \rightarrow Y\ and \ X \rightarrow Z$

You can break down a set of dependencies 

> [!EXAMPLE]
> If $SSN \rightarrow \{Name, Salary\}$, $SSN \rightarrow Name\ and \ SSN \rightarrow Salary$

## Additive
> If $X \rightarrow Y \land X \rightarrow Z$, $X \rightarrow \{Y,Z\}$

You can combine dependencies into one set

# Full dependency
A full dependency means that no [[Keys#Prime attributes|prime attributes]] can be removed from the key

> $X \ \backslash \{A\} \not \rightarrow Y$
> If you remove the prime attribute $A$ from $X$, $X$ no longer functionally determines $Y$

## Verbosity
A key is verbose if it has prime attributes that don't need to be part of it