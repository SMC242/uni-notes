---
tags: django
---

# Overview
- Templates are skeletons that are filled with data by [[Views]]

# Configuration
- Go to your project root
	- `mkdir templates`
- For each app:
	- `mkdir templates/<app name>`
- Go to `<project name>/settings.py`
	- In the paths section, add `TEMPLATE_DIR = os.path.join(BASE_DIR, "templates")`
	- In `TEMPLATES`, add `TEMPLATE_DIR` to the list in the `DIRS` key
```python
TEMPLATES = [
	{
		"DIRS": [TEMPLATE_DIR],
	},
]
```

> [!INFO]
> The `os.path.join` wizardry is to avoid hard-coding paths. If the paths were hard-coded, they would break between developers and across operating systems

# Syntax
- HTML with [handlebars](https://handlebarsjs.com/) embedded

## Template tags
These tell Django to do commands and are formatted like this:
`{% <command name> <command arguments> %}`

See: [Template Tag Documentation](https://docs.djangoproject.com/en/4.1/ref/templates/builtins/#load)

# Usage
`templates/<app name>/greeter.html`:
```html
<html>
	<body>
		<p>Hello, {{ name }}</p>
	</body>
</html>
```

`<app name>/urls.py`:
```python
from django.shortcuts import render
from django.http import HttpResponse, HttpRequest

def greeter(request: HttpRequest) -> HttpResponse:
	context = {"name": "Jeff"}
	return render(
		request,
		"<app name>/greeter.html",
		context=context
	)
```

See also: [[Bootstrapping]]