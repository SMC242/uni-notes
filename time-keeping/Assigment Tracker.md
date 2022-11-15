# Active assignments
```dataview
TABLE
	class, dateformat(deadline, "yyyy-MM-dd @hh:mm") AS deadline, difficulty, time, progress
FROM
	"assignments"
SORT
	deadline ASC
```

# Assignment history
```dataview
TABLE
	class,
	dateformat(deadline, "yyyy-MM-dd") as submitted,
	round((marks / max-marks) * 100, 2) + "%"  as score
FROM
	"assignments"
WHERE
	progress = "Finished"
```