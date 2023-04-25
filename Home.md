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
LIST
	file.path
FROM
	"/"
WHERE
	icontains(file.name, "map")
```

# Courses
```dataview
LIST
	file.path
FROM
	"/"
WHERE
	icontains(file.name, "dashboard")
```
