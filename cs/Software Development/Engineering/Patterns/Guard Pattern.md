# Motivation
Using the guard pattern reduces indentation and cognitive load when reading functions with conditions

# The pattern
- Flip `if` statements to handle the `else` clause first

## Examples
```python
# Before
res = await get_wiki_page("James Maynard Keenan")
if res.status == 200:
	page = res.data
	if "present" in page["Background information"]["Years active"]:
		print("James Maynard Keenen is still active")
else:
	raise RuntimeError(
		f"Request failed with status {res.status}"
		)
```

```python
# After
res = await get_wiki_page("James Maynard Keenan")
if res.status != 200:
	raise RuntimeError(
		f"Request failed with status {res.status}"
		)

page = res.data
if "present" in page["Background information"]["Years active"]:
	print("James Maynard Keenen is still active")
```

### Extreme example
```java
// Before
public void Subscribe(User user, Subscription subscription, Term term)
{
    if (user != null)
    {
        if (subscription != null)
        {
            if (term == Term.Annually)
            {
                // subscribe annually
            }
            else if (term == Term.Monthly)
            {
                // subscribe monthly
            }
            else
            {
                throw new InvalidEnumArgumentException(nameof(term));
            }
        }
        else
        {
            throw new ArgumentNullException(nameof(subscription));
        }
    }
    else
    {
        throw new ArgumentNullException(nameof(user));
    }
}
```

```java
// After
public void Subscribe2(User user, Subscription subscription, Term term)
{
    if (user == null)
    {
        throw new ArgumentNullException(nameof(user));
    }
    if (subscription == null)
    {
        throw new ArgumentNullException(nameof(subscription));
    }
    if (term == Term.Annually)
    {
        // subscribe annually
    }
    else if (term == Term.Monthly)
    {
        // subscribe monthly
    }
    else
    {
        throw new InvalidEnumArgumentException(nameof(term));
    }
}
```