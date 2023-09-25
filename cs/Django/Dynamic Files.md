---
aliases:
- Media
tags: Django

---
# Overview
- Media is any dynamic file, as opposed to a [[Static Files#Overview|static file]]
- This is often user-generated content like avatars or images of products

# Configuration
- Go to your project root
	- `mkdir media`

## Media paths
- Go to `<project name>/settings.py`
	- In your paths section, add `MEDIA_DIR = os.path.join(BASE_DIR, "media")`
	- At the bottom, add a section for media (`# Media files`)
	- Add the following: `MEDIA_ROOT = MEDIA_DIR` and `MEDIA_URL = "/media/"`

> [!WARNING]
> There must be a forward slash in `MEDIA_URL`

## Context processors
A context processor is used to provide access to URLs like `MEDIA_URL`. This will be important for finding the correct path for media files

- Go to `<project name>/settings.py`
	- In the `TEMPLATES` config, add `"django.template.context_processors.media"` to `OPTIONS.context_processors`
```python
TEMPLATES = [
	{
		"OPTIONS": {
		"context_processors": [
			"django.template.context_processors.media",
			],
		},
	},
]
```

## Mapping URL
You still need to map a URL for media
- Go to `<project name>/urls.py` and add the following imports:
```python
from django.conf import settings  # Gives access to the constants in `settings.py`
from django.conf.urls.static import static
```
- Concatenate `urlpatterns` with `static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)`
```python
urlpatterns = [
			   ...
] + static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)
```

# Usage
`templates/<app name>/jerma.html`:
```html
<html>
	<body>
		<h1>An epic picture of Jerma</h1>
		<img src="{{ MEDIA_URL }}jerma.png" alt="Jerma :^)" />
	</body>
</html>
``````