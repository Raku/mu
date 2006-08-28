use v6-alpha;

use Test;

plan 1;

# Note: There's *no* ";" before the "\n", but it parses nevertheless!
ok !eval("42 if 23\nis 50; 1"),
    "if postfix modifier and is() is parsed correctly";
