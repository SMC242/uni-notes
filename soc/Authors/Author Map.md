# Overviews
```dataview
LIST
	description
FROM
	"soc/Authors"
WHERE
	file.name != this.file.name
```

# Socio-economics
These authors talk about the interactions between social (ethnicity, gender, social class) and economic (wealth, location) factors 

## Class
Social class is how people in society are divided. This may be due to economic status, birth circumstances (caste system), ethnicity, etc
```dataview
LIST
FROM
	#Class
```

## Capitalism
Capitalism is an economic system where people can the means of production (things used to make things) and aim for profit
```dataview
LIST
FROM
	#Capitalism 
WHERE
	contains(file.path, "Authors")
```

# Society
- Defining society
- How it changes
- How it is maintained

```dataview
LIST
FROM
	#Modernity OR #Civilisation 
WHERE
	contains(file.path, "Authors")
```

# Power
- Defining power
- What creates power
- Who controls power

- [[Karl Marx]]
- [[Michel Foucault]]
- [[Max Weber]]
# Gender
These authors discuss gender

## Feminism
Feminism is a movement that advocates for women's rights
```dataview
LIST
FROM
	#Feminism
WHERE
	contains(file.path, "Authors")
```

# Ethinicity
Ethnicity is a grouping of perceived characteristics such as culture, language, religion, and history

