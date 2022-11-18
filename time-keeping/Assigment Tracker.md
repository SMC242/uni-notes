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
	choice(marks != null AND max-marks != null, round(marks / max-marks, 2), "N/A") as score
FROM
	"assignments"
WHERE
	progress = "Finished"
```