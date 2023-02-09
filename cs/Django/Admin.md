# Overview
Django offers an admin dashboard for managing collections. However, it doesn't know about your application's [[Django Databases#Models|models]] by default

# Configuration
- Go to `<project name>/admin.py`
	- Import the models from each app

Models can be loaded in a few ways. You should use [[#Custom listing]] when you want to control how records are displayed. Otherwise, use [[#Simple loading]]. Pick a strategy per model

## Simple loading
This will create a basic display
```python
from django.contrib import admin

models = [
		  Model1,
		  Model2,
]

for m in models:
	admin.site.register(m)
```

## Custom listing
```python
from django.contrib import admin

@admin.register(Model1)
class Model1Admin(admin.ModelAdmin):
	# Control which fields are displayed
	list_display = ("field1", "field2", "field3")
```