
```button
name Create new assignment
type command
action QuickAdd: Create new assignment
color purple
class button-overrides
```

# Active assignments
```dataview
TABLE
	class, dateformat(deadline, "yyyy-MM-dd @HH:mm") AS deadline, difficulty, time, progress
FROM
	"assignments"
WHERE
	progress != "Finished"
SORT
	deadline ASC
```

# Possibly outdated
```dataview
TABLE 
	class,
	dateformat(deadline, "yyyy-MM-dd @HH:mm") AS deadline,
	"Y" + year + "S" + semester AS when,
	progress
FROM
	"assignments"
WHERE
	progress != "Finished" AND deadline < date(now)
```

# Assignment history
```dataview
TABLE
	class,
	"Y" + year + "S" + semester AS when,
	dateformat(deadline, "yyyy-MM-dd") as submitted,
	round(default(marks, 0) / default(max-marks, 0), 2) * 100 + "%" as score
FROM
	"assignments"
WHERE
	progress = "Finished"
SORT
	deadline DESC
```