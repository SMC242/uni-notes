---
tags: Django

---
# Overview
- Databases persist data
- Django is built on top of SQL databases such as [SQLite](https://www.sqlite.org/index.html)
- It provides an [[ORM]] to abstract away the queries

# Configuration
## Database backend
By default, Django uses SQLite, but that can be changed using this configuration in `<project name>/settings.py`:
```python
DATABASES = {
	"default": {
		"ENGINE": "django.db.backends.sqlite3",
		"NAME": os.path.join(BASE_DIR, "db.sqlite3"),
	}
}
```

## Superuser
A superuser is basically an admin

In your project root, run `python manage.py createsuperuser`

## Admin dashboard
![[Admin Dashboard]]

# Models
![[Models]]

# Migrating
After you've updated an [[Apps|app]]'s `models.py`, you need to update the database to use the new models

- Go to your project root
	- Run `python manage.py makemigrations <app name>`
	- Run `python manage.py migrate`

## What's going on under the hood?
You can see the generated SQL with `python manage.py sqlmigrate <number>`

# Database REPL
Go to your project root and run:
`python manage.py shell`

This will allow you to play with [[ORM]] queries
```python
>>> from rango.models import Category
>>> print(Category.objects.all())  # Query all records in Category
>>> Category(name="Melodic Death Metal").save()  # Create a new record in Category
>>> quit()  # Exit the REPL
```

# Populating
![[Populating]]

# Re-intialising
- Go to your project root
	- `rm db.sqlite3`
	- `python manage.py migrate` to create the database with your models
	- `python manage.py createsuperuser` to create a new admin
	- `python <population script>.py` to fill your database

# See also
- [[ORM Methods]]]