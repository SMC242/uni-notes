# Trackers
```dataview
LIST
	file.path
FROM
	"/"
WHERE
	icontains(file.name, "tracker")
```

# Maps of content
## Top-level
- [[CS Map|Computing Science]]
- [[Maths Map|Mathematics]]
- [[Stats Map|Statistics]]
- [[Research Map|Research]]
- [[Sociology Map|Sociology]]
- [[Archaeology Map|Archaeology]]
- [[Plant Map|Plants]]
- [[Prompt Map|Prompts]]
- [[Daily Note Map]]

## All
```dataview
TABLE
	file.folder as "folder"
FROM
	"/"
WHERE
	icontains(file.name, "map")
```
