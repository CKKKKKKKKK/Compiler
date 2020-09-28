# Documentation

### 1.  Handle comments

​	I add a global variable comment_counter in tiger.lex. If a "/*" is met, then comment_counter will be added by one. And if it is in the INITIAL state, the state will be changed to COMMENT; if it is in COMMENT state, then there is no change on state. If a "*/" is met, then comment_counter will be subtracted by one. And if comment_counter is equal to 0, then the state will be changed from COMMENT to INITIAL.

### 2. Handle strings

​	To handle strings, I implemented function getstr to translate all the escape sequences in the strings into their meaning and then assign the result of getstr to sval.



### 3. Error handling

​	At the end of the lists of regular expressions I add a regular expression to catch illegal string and print the error message and the position of the illegal string.



### 4. End-of-file handling

​	To handle end-of-file, I add a regular expression

```
<INITIAL><<EOF>> {adjust(); yyterminate();}
```

