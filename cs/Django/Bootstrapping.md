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
- Go to `<project name>/urls.py`
	- Map a URL onto the new app
	- Practically, this means adding `path("<app name>", include("<app name>.urls")` to `urlpatterns`
- Go to `<app name>/urls.py`
	- Map some urls to some views
	- Practically, this means adding `path("example", views.example)` to `urlpatterns`
- Go to `<app name>/views.py`
	- Create some [[Views]]
- Go to the project root
	- Run `python manage.py runserver localhost:5555`