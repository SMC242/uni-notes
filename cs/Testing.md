# Overview
Automated testing aims to throw a variety of correct and incorrect data at a program to make sure it is robust. It is impossible to cover every possible case - tests are limited by the programmer's imagination - but they can eliminate obvious issues

# Definitions
<dl>
    <dt>Unit test case</dt>
    <dd>A single scenario for testing a function</dd>
    <dt>Test case</dt>
    <dd>A test</dd>
    <dt>Test suite</dt>
    <dd>A set of test cases</dd>
    <dt>Test runner</dt>
    <dd>Runs a set of test suites and reports any failures</dd>
</dl>

# Test types
## Unit tests
- Testing small blocks of code such as a single function
- This finds the root cause of system failures

Example: Python's [unittest](https://docs.python.org/3/library/unittest.html) or Java's [JUnit](https://junit.org/junit5/docs/current/user-guide/). Tests may also be integrated into a function's documentation, such as in Python's [doctest](https://docs.python.org/3/library/doctest.html)

## Integration tests
- Testing a group of functions end to end
- This often uncovers state errors or mismatched function types

## Property-based testing
- A type of testing that randomly generates test data based on the function's type signature

Example: Haskell's [QuickCheck](https://hackage.haskell.org/package/QuickCheck)

## GUI testing
- Automated interactions with a GUI
- Common in the front-end Javascript world with libraries such as [Jest](https://jestjs.io/) and [Puppeteer](https://pptr.dev/)

# What to test
- Functionality: does it meet the functional requirements?
- Usability: ease of use
- Security/penetration testing: can it be broken into by bad actors?
- Performance: how fast is it? What is the memory footprint? What is the carbon footprint?
- Portability: which operating systems/browsers is the program compatible with?
- Reliability: how resilient is the program? How quickly can it recover from a crash?

# Making good tests
Check:
- Edge cases
- Try lots of different inputs
- Try empty, negative, and positive values
	- This will be different depending on the types of the function
	- Use empty strings, empty lists, negative numbers, `0`, nulls
- Try to cause errors

# Reliability testing
Test what happens if:
- The client times out
- The database loses connection or there are network issues
- Downtime: a section of the backend goes down
- Recovery: what happens after the app goes down? Are there fallbacks?

# Chaos engineering
Deliberately causing faults to see how the system reacts. This might mean:
- Causing latency
- Taking subsystems offline
- Crashing database connections
- Taking down servers
- Simulating high traffic

# Accessibility testing
- Frontends must be tested to see if people with disabilities can use them
- [i18n](https://www.i18next.com/) is a project for internationalisation
- [Aria](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA) is a set of rules to make sure websites are accessible
- Frontends should be tested on a variety of devices (E.G phones, laptops, tablets), browsers, and operating systems
	- Network speed may also be a consideration

# A/B testing
Sometimes, companies will deploy a new version of their app to a subset of users. They will measure the response to the new version vs the old and decide whether to deploy the changes to the whole userbase

