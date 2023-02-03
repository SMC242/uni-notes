---
tags: django
---

# Initialising
`python django-admin startproject <name>`
The naming convention is `*_project`


# New apps
One project can have many apps

- `cd` into your [[#Initialising|project directory]] (the project root)
- `python manage.py startapp <app name>`
- Go to `<project name>/settings.py`
	- Add `<app name>` to `INSTALLED_APPS`
- Go to your project root
	- Run `python manage.py migrate`

# Running
- Go to the project root
	- Run `python manage.py runserver localhost:5555`

# First route
- Go to `<project name>/urls.py`
	- Map a URL onto the new app
	- Practically, this means adding `path("<app name>", include("<app name>.urls")` to `urlpatterns`
- Go to `<app name>/urls.py`
	- Map some URLs to some views
	- Practically, this means adding `path("example", views.example)` to `urlpatterns`
- Go to `<app name>/views.py`
	- Create some [[Views]]

# Adding templates
![[Templates#Configuration]]

# Adding static files
![[Static Files#Configuration]]

# Adding dynamic files
![[Dynamic Files#Configuration]]

# Resulting structure
```
example_project
├── db.sqlite3
├── example
│  ├── __init__.py
│  ├── admin.py
│  ├── apps.py
│  ├── migrations
│  │  └── __init__.py
│  ├── models.py
│  ├── tests.py
│  └── views.py
├── example_project
│  ├── __init__.py
│  ├── settings.py
│  ├── urls.py
│  └── wsgi.py
├── manage.py
├── media
├── static
│  └── images
└── templates
   └── example
```
- The top-level `example_project` is the project root
- `example_project` is the project settings folder
- `example` is *an* app folder
	- See [[Apps]] for more information on the distinction between apps and projects
- 