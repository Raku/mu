#!/usr/bin/pugs

use v6;
require Test;

plan(23);

# Simple - with just a single char

todo_is(rindex("Hello World", "H"), 0, "One char, at beginning");
todo_is(rindex("Hello World", "l"), 9, "One char, in the middle");
todo_is(rindex("Hello World", "d"), 10, "One char, in the end");
todo_is(rindex("Hello World", "x"), -1, "One char, no match");

todo_is(rindex("Hello World", "l", 10), 9, "One char, first match, pos @ end");
todo_is(rindex("Hello World", "l", 9), 9, "- 1. match again, pos @ match");
todo_is(rindex("Hello World", "l", 8), 3, "- 2. match");
todo_is(rindex("Hello World", "l", 2), 2, "- 3. match");
todo_is(rindex("Hello World", "l", 1), -1, "- no more matches");

# Simple - with a string

todo_is(rindex("Hello World", "Hello"), 0, "Substr, at beginning");
todo_is(rindex("Hello World", "o W"), 4, "Substr, in the middle");
todo_is(rindex("Hello World", "World"), 6, "Substr, at the end");
todo_is(rindex("Hello World", "low"), -1, "Substr, no match");
todo_is(rindex("Hello World", "Hello World"), 0, "Substr eq Str");

# Empty strings

todo_is(rindex("Hello World", ""), 11, "Substr is empty");
todo_is(rindex("", ""), 0, "Both strings are empty");
todo_is(rindex("", "Hello"), -1, "Only main-string is empty");
todo_is(rindex("Hello", "", 3), 3, "Substr is empty, pos within str");
todo_is(rindex("Hello", "", 5), 5, "Substr is empty, pos at end of str");
todo_is(rindex("Hello", "", 999), 5, "Substr is empty, pos > length of str");

# More difficult strings

todo_is(rindex("abcdabcab", "abcd"), 0, "Start-of-substr matches several times");  

todo_is(rindex("uuúuúuùù", "úuù"), 4, "Accented chars");
todo_is(rindex("Ümlaut", "Ü"), 0, "Umlaut");

