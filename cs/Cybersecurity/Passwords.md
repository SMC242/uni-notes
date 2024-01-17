# Overview
An [[Authentication]] method using passphrases

- The user provides an identifier (ID) and a password
- The system compares the password to the stored password for that user ID
	- Stored in a password file
- The ID is useful for:
	- Indicating whether the user is authorised to access the system
	- Finding the user's privileges within the system
	- Other users may grant that ID permissions

# Vulnerabilities
- Specific account attacks: the attacker guesses a targeted account's password until correct
	- The attacker may research the target to inform their guesses
- Popular password attacks: taking a list of common passwords and applying it to many user IDs
- Workstation hijacking: the attacker accesses a machine that has been left logged-in
- Exploiting mistakes
	- If the system created the password, the user is more likely to write it down
	- The user might share the password
	- [[Social Engineering]]
- Exploiting repeated password usage: users may re-use passwords across various systems

# Countermeasures
- Lock out after repeated failed logins
- Auto-logout after inactivity
- Intrusion detection (detecting changes in user behaviour)
- Forbidding weak or re-used passwords
- Strong passwords
	- At least 12 characters
	- Large character set
	- No personal information
	- Passwords must be changed often

# Hashing
- Storing passwords in cleartext is bad
- The hashing algorithm must be slow 
	- To prevent brute-force attacks

See also: [[Hashing]]

## Process
### Initialisation
1. Password selected
2. Salt added to password
	- A fixed-length  random value
3. Password + salt combined hashed
4. Hashed password + plaintext salt is stored in the password file

### Verifying
1. User provides ID and password
2. Plaintext salt and hashed password retrieved from the password file
3. The provided password + salt is hashed
4. The result is compared to the stored hash

## Salts
- Prevents duplicate passwords from having the same hash
- Makes it hard to see if a user is using the same password in other systems
- Makes offline dictionary attacks difficult
	- Hackers often find password files
	- If salting is not used, the password file can be compared against a rainbow table
	- Salting means that the attacker has to pre-compute their [[#rainbow tables]] for each possible salt value
		- Not feasible

#### Rainbow tables
- A precomputed hash table of passwords to hashes
	- Multiple possible salt values are used
- Trades off space for time
	- The storage cost of such a table is immense
- If the salt is large enough (E.G 128 bits) this becomes unfeasible

# Password cracking
- Generating passwords based on the probabilities of letters in natural language
- Uses datasets of leaked passwords as training data
	- Training probabilistic context-free grammar model

# Shadow password file
- When hashed passwords are kept separate from user IDs
- Accessible only by the root user

# Selecting good passwords
- User education
- Computer-generated passwords
	- Poor acceptance rate by users
- Reactive password checking
	- The system periodically attempts to crack its users passwords
	- Users with easy passwords are told to change them
	- Defenders have a disadvantage because they can't spend all of their resources on this process (they have a system to run too). Attackers can
- Proactive password checking
	- The user picks their password
	- The system checks the password against:
		- Some criteria (E.G has a number, special character, capital letter)
- Password manager
	- Software that generates and stores secure passwords
	- The user has to remember only the master password
		- This password should be extremely strong
	- Single point of failure
		- The password manager could be breached
		- The user might write down their master password
		- The user might forget their master password and be locked out of all their accounts