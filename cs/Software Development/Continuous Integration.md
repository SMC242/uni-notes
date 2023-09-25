---
aliases: CI

---
# Definition
Continuous integration is automation of tasks such as compiling when new code is pushed to a [[Git#Repository|repository]]

# Pipelines
- Git hosting solutions such as [Github](https://github.com) and [Gitlab](https://about.gitlab.com/) allow you to set up programs to run on commit or merge
- These may run linters, tests, and automatically deploy the code

# Integration hell
When it takes longer to [[Git#Merge|merge]] new code than it did to write it. This can be for a variety of reasons:
- Long compile times
- Slow tests
- Bloated dependencies taking a long time to install