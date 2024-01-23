---
tags: AccessControl
---
# Overview
The [[Access Control]] system used by UNIX-like operating systems. Built on top of [[Discretionary Access Control]]

# Modes
- User mode: some regions of memory are not allowed to be accessed and some instructions are banned
- Kernel mode:  privelleged instructions can be executed and protected memory can be accessed 

# Inodes
AKA index nodes

- Contains key information about the file
	- Size
	- Permissions
	- User and group IDs
- Multiple file names can be associated with one inode (hard links)
- Each file is controlled by 1 inode
- An inode table is maintained 
	- When a file is opened, the inode is loaded into main memory

## Directories
- A file that contains a list of file names and pointers to their inodes
- Can be nested
- Each directory has its own inode

## Access control
- Users are assigned a unique ID
- A user is part of one primary group and possibly some other groups
	- Each group has an ID
- When files are created, the creator is marked as the owner (their ID is stored in the inode)

## Protection bits
![Protection bits](https://helpdeskgeek.com/wp-content/pictures/2017/02/file-permissions-explanation.png)

- The first bit is a directory flag
- It's followed by 9 protection bits per inode
- 3 segments
	- Each segment has 3 bits representing permissions:
		- `r`: read (list files)
		- `w`: write (create, rename, delete)
		- `x`: execute (go into the directory and search it)
	- Segments in order: owner, group, other

### Special bits
The `x` bit in each segment can be replaced with a special permission

In order:
- SetUID (owner): when a user with execute privileges executes the file, the user temporarily gains the rights of the creator 
	- `chmod u+s`
- SetGID (group): when a user with execute privileges executes the file, the user temporarily gains the rights of the file's group
	- `chmod g+s`
	- If applied to a directory, new files will inherit the directory's group
- Sticky bit (other): specifies that files in a directory can be renamed, moved, or deleted by the owner only
	- `chmod o+t`

### Numeric representation
- Use up to 4 octal digits
	- The codes for each permission are summed to create the digit
	- First digit: set user ID = 4, set group ID = 2, sticky bit = 1
	- Digits 2-4: permissions. Read = 4, write = 2, execute = 1
- An omitted bit is interpreted as a leading 0

> [!EXAMPLE]
> - `7777`: anyone can read, write, and execute
> 	- `rwsrwsrwt`
> - `0644`: readable by everyone, can only be modified by the owner
> 	- `rw-r--r--`
> - `4750`: owner can read, write, execute. Group can read, execute. Set ID is enabled
> 	- `rwsr-x---`

# Superuser
AKA root access

- A special account for administration
	- Exempt from the usual file access control mechanisms
	- Unrestricted access to all files, directories, and resources
	- Can grant/remove permissions for other users
- Very few people should know the password and the password should be very strong

