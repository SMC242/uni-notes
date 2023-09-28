# Overview
[Git](https://git-scm.com/) is a version control system. It is used to allow developers to make changes in isolation of each other and help them to [[#merge]] their code. Git hosting services such as [Github](https://github.com) and [Gitlab](https://about.gitlab.com) provide [[#remote]] storage and additional features such as [[#kanban boards]], [[#Issue|issue tracking]], and [[Continuous Integration]]

# Commit
Commits are the fundamental unit in Git. They contain a set of changes. By breaking up changes into small commits, you can undo specific changes or rewind to a certain point in history

Each commit has a message that should summarise its changes

# Repository
Where code is stored. It will have many files, directories, and [[#Branches]]

# Branch
A branch is an isolated timeline that breaks off from another branch. Usually, there is a production branch, testing branch, and various feature and bug branches

The convention is to use forward slashes in branch names for features and bugs. Example: `feature/navbar` or `bug/incorrect-usernames`. You may also add an [[#issue]] ID into the name if relevant

<dl>
	<dt>Production</dt>
	<dd>The code that is to be deployed</dd>
	<dt>Testing</dt>
	<dd>A newer version of production that is undergoing final testing before deployment</dd>
	<dt>Feature</dt>
	<dd>A branch that adds a new function to the program</dd>
	<dt>Bug</dt>
	<dd>A branch that fixes an issue in the program</dd>
</dl>

# Merge
The process of combining the histories of two [[#branch]]es

# Diffing
Comparing two versions of the same file to see how it has changed
![Diff example](https://sgeb.io/posts/git-diff-anywhere/git-diff.png)

# Issue
A document that specifies a problem, enhancement, or suggestion for the program

# Kanban board
A method of issue tracking. They're divided into various headings such as "to do", "in progress", "in testing", and "finished". [[#Issue]]s are moved between headings as they are worked on

![Kanban board example](https://upload.wikimedia.org/wikipedia/commons/f/f5/Kanban_board_example.jpg)

# Triage
Triage is the process of assigning priorities to [[#issue]]s. Some features and bugs are more important than others and must be worked on first