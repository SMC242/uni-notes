---
tags: Django 
---

# Motivation
## Demos
Fake data is quick to generate, but not ideal for demos. Instead, a population script may be used to insert pre-defined data into the database

## Testing
When testing your code, it's helpful to have data already in the database to work with. A population script is a portable way to create test data

# Example
```python
# The order of the imports is weird because the environment
# variable needs to be set up before any Django modules are loaded
import os
os.environ.setdefault("DJANGO_SETTINGS_MODULE", "rango_project.settings")
import django
django.setup()

from rango.models import Category, Page

CATEGORIES = {
			  "Category1": [
				  {"title": "Page One",
				  "url": "https://example.com"}
			  ]
}

def add_category(name: str) -> Category:
	c, _ = Category.objects.get_or_create(name=name)
	c.save()
	return c

def add_page(category: Category,
			 title: str,
			 url: str,
			 views: int = 0) -> Page:
	p, _ = Page.objects.get_or_create(
		category=category,
		title=title
		)
	p.url, p.views = url, views
	p.save()
	return p
    
def populate():
	for category_name, page_list in CATEGORIES.items():
		category = add_category(category_name)
		for page in page_list:
			add_page(category, page["title"], page["url"])
```