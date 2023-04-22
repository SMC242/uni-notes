# Purpose
- Creating progress bars from the metadata of a file

# Usage
1. Add the following keys to the file's front-matter:
```yaml
progress: 0
target: 100
```

These will represent your progress through the task.

2. Insert a dataviewjs block with the following code:
```
await dv.view("scripts/progress-bar", {file: "{file name}"});
```

3. Change `{file name}` to the path to the file

# Example
Front-matter:
```yaml
progress: 48
target:136
```

Dataview.js block:
```dataviewjs
await dv.view("scripts/progress-bar", {file: "cs/WD2 Dashboard"});
```

# Implementation
[[progress-bar.js]] pulls the keys from the front-matter in order to calculate the current percentage. It then generates the HTML and returns it via [Dataview](https://blacksmithgu.github.io/obsidian-dataview/queries/dql-js-inline/)
Source for original code: https://beingpax.medium.com/how-to-create-a-visual-progress-bar-in-obsidian-3016416dad19