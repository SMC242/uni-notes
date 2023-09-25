---
tags: Django

---
# Overview
- A [[MVC#View|view]] is a function that returns HTML to the client

# Django code
## Minimal implementation
```python
from django.http import HttpResponse, HttpRequest

def index(request: HttpRequest) -> HttpResponse:
    return HttpResponse("")
```

# Parameters
Some views take parameters in their URL such as https://tenor.com/search/tag1+tag2+tag3 - the tags are parameters

In Django, such a view would be defined as follows:
```python
from django.http import HttpResponse, HttpRequest

def search(request: HttpRequest, tag_param: str) -> HttpResponse:
	tags = tag_param.split("+")
	...
```

You'd then have to edit `<app name>/urls.py` like so:
```python
from django.urls import path

from app_name import views

urlpatterns = [
			   path(
				   "search/<string:tag_param>",
				   views.search,
				   name="search",
			   )
]
```

# See also
- [[Templates]]