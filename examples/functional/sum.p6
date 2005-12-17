multi sub sum ()          returns Int { 0      }
multi sub sum (Array *@x) returns Int { [+] @x }

say "... sum";
say sum(1 .. 10);
say sum(1 .. 5);
say sum(2, 2, 2, 2);

