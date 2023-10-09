---
tags: Django

---
# Overview 
- Static files are any files that aren't dynamically generated
- CSS, JS, and most media are all static

# Configuration
- Go to your project root
	- `mkdir static`
	- `mkdir static/images`
- Go to `<project name>/settings.py`
	- In the paths section, add `STATIC_DIR = os.path.join(BASE_DIR, "static")`
	- Go to the static files section (at the bottom)
	- Add `STATICFILES_DIRS = [STATIC_DIR,]`
	- Check that `STATIC_URL = "/static/"`

> [!WARNING]
> `STATIC_URL` must end in a forward slash

# Images
To add an image:
- Put the file in `/static/images`
- In your chosen template:
	- Add `{% load staticfiles %}` to the top of the file
	- Add an `img` tag like so:
```html
<img src="{% static 'images/<image name>' %}" />
```

See also: [[Bootstrapping]]