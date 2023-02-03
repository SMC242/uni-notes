---
tags: django
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