multi sub sum ()          returns Int { 0             }
multi sub sum (*$x, *@xs) returns Int { $x + sum(|@xs) }

say "... sum";
say sum(1 .. 10);
say sum(1 .. 5);
say sum(2, 2, 2, 2);

