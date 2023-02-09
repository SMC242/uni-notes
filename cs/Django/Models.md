---
tags: django 
---

# Models
- Models are how you interact with the database

See also: [[ORM]]

# Minimal example
```python
from django.db import models

class TableName(model.Model):
	# Field declarations here
	example_name_field = models.CharField()

	def __str__(self) -> str:
		"""
		Return a field that represents the instance.
		Useful when debugging
		"""
		return self.example_name_field
```

# Field types
The main types are:
- `CharField([max_length: int])`
- `URLField([max_length: int])`
- `IntegerField()`
- `DateField()`

See also: [field documentation](https://docs.djangoproject.com/en/2.1/ref/models/fields/#model-field-types)

## Optional parameters
- `unique: bool` - whether the value should be unqiue
- `default: T` - the default value if not specified
- `null: bool` - whether the field is nullable

# Relationships
- `ForeignKey(table_name: str)`
- `OneToOneField(table_name: str)`
- `ManyToManyField(table_name: str)`

Usually, you'll use `on_delete=models.CASCADE` to make sure records get deleted in all tables

## Optional parameters
See: [relationship documentation](https://docs.djangoproject.com/en/2.1/ref/models/fields/#django.db.models.CASCADE)

# Using slugs
Often blogs will have titles containing spaces. Unfortunately, encoding spaces in a URL results in ugly `%20` sequences. To get around this, spaces are replaced with hyphens ^a83e70

Django provides a function for this:
```python
>>> from django.template.defaultfilters import slugify
>>> slugify("I am very boring")
'i-am-very-boring'
```

Here is how you can set up a model to automatically create slugs in `<app name/models.py`:
```python
from django.db import models
from django.template.defaultfilters import slugify

class BlogPost(models.Model):
	title = models.CharField(max_length=128, unique=True)
	slug = models.SlugField(unique=True)

	def save(self, *args, **kwargs):
		self.slug = slugify(self.title)
		super(BlogPost, self).save(*args, **kwargs)
```

## Fixing admin page
To auto-generate slugs in the [[Admin Dashboard]]:
- Go to `<app name>/admin.py`
	- Create a `ModelAdmin` for the model that uses slugs
```python
from django.contrib import admin

from app_name.models import BlogPost

@admin.register(BlogPost)
class BlogPostAdmin(admin.ModelAdmin):
	prepopoulated_fields = {"slug": ("name",)}
```