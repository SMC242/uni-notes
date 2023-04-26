---
tags: Django 
---

# Queries
These methods fetch data from the database

## all
```python
records = Table.objects().all()
```

Gets all records for `Table`

## filter
```python
records = Table.objects().filter(x=x, y=y)
```

Filters records by the specified fields

## order_by
```python
Table.objects.order_by("field")
```

Gets all records and sorts them by `field` ascending. Use a hyphen to sort descending (`"-field"`)

# Mutations
These methods create or update data in the database

## get_or_create
```python
instance, created = Table.objects.get_or_create(field=value)
```

Checks to see if a record with `field=value` exists:
- True: returns the record without creating a new one
- False: creates and returns a new record

`created` indicates which path was taken