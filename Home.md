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
```dataview
TABLE
	file.folder as "folder"
FROM
	"/"
WHERE
	icontains(file.name, "map")
```
