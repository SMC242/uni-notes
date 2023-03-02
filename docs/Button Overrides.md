# Purpose
- Custom styles for buttons generated with the [Buttons Plugin](https://github.com/shabegom/buttons)

# Features
- Removes green outline

# Usage
When creating a button, add `class button-overrides` to use the overrides

```
name Save
type command
action Save current file
class button-overrides <-- add this line
```

# Example
```button
name Save
type command
action Save current file
class button-overrides
```