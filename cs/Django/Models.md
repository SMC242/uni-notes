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