#!/usr/bin/pugs

use v6;
require Test;

plan(23);

# Simple - with just a single char

ok(index("Hello World", "H") == 0, "One char, at beginning");
ok(index("Hello World", "l") == 2, "One char, in the middle");
ok(index("Hello World", "d") == 10, "One char, in the end");
ok(index("Hello World", "x") == -1, "One char, no match");

ok(index("Hello World", "l", 0) == 2, "One char, find first match, pos = 0");
ok(index("Hello World", "l", 2) == 2, "- 1. match again, pos @ match");
ok(index("Hello World", "l", 3) == 3, "- 2. match");
ok(index("Hello World", "l", 4) == 9, "- 3. match");
ok(index("Hello World", "l", 10) == -1, "- no more matches");

# Simple - with a string

ok(index("Hello World", "Hello") == 0, "Substr, at beginning");
ok(index("Hello World", "o W") == 4, "Substr, in the middle");
ok(index("Hello World", "World") == 6, "Substr, at the end");
ok(index("Hello World", "low") == -1, "Substr, no match");
ok(index("Hello World", "Hello World") == 0, "Substr eq Str");

# Empty strings

ok(index("Hello World", "") == 0, "Substr is empty");
ok(index("", "") == 0, "Both strings are empty");
ok(index("", "Hello") == -1, "Only main-string is empty");
ok(index("Hello", "", 3) == 3, "Substr is empty, pos within str");
ok(index("Hello", "", 5) == 5, "Substr is empty, pos at end of str");
todo_ok(index("Hello", "", 999) == -1, "Substr is empty, pos > length of str");

# More difficult strings

ok(index("ababcabcd", "abcd") == 5, "Start-of-substr matches several times");  

ok(index("uuúuúuùù", "úuù") == 4, "Accented chars");
ok(index("Ümlaut", "Ü") == 0, "Umlaut");


