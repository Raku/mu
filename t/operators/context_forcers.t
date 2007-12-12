use v6-alpha;

use Test;

plan 36;

{ # L<S03/"Operator renaming" /imposes a boolean context/>
  is ?True,    True,  "? context forcer works (1)";
  is ?False,   False, "? context forcer works (2)";

  is ?1,       True,  "? context forcer works (3)";
  is ?0,       False, "? context forcer works (4)";
  is ?(?1),    True,  "? context forcer works (5)";
  is ?(?0),    False, "? context forcer works (6)";

  is ?"hi",    True,  "? context forcer works (7)";
  is ?"",      False, "? context forcer works (8)";
  is ?(?"hi"), True,  "? context forcer works (9)";
  is ?(?""),   False, "? context forcer works (10)";

  is ?"3",     True,  "? context forcer works (11)";
  is ?"0",     False, "? context forcer works (12)";
  is ?(?"3"),  True,  "? context forcer works (13)";
  is ?(?"0"),  False, "? context forcer works (14)";
}

{ # L<S03/"Operator renaming" /imposes a numeric context/>
  is +1,           1, "+ context forcer works (1)";
  is +0,           0, "+ context forcer works (2)";
  is +"1",         1, "+ context forcer works (3)";
  is +"0",         0, "+ context forcer works (4)";
  is +"",          0, "+ context forcer works (5)";
  is +undef,       0, "+ context forcer works (6)";
  is +"Inf",     Inf, "+ context forcer works (7)";
  is +"-Inf",   -Inf, "+ context forcer works (8)";
  is +"NaN",     NaN, "+ context forcer works (9)";
  is +"3e5",  300000, "+ context forcer works (10)";
  is +(?0),        0, "+ context forcer works (11)";
  is +(?3),        1, "+ context forcer works (11)";
}

{ # L<S03/"Operator renaming" /imposes a string context/>
  is ~1,         "1", "~ context forcer works (1)";
  is ~0,         "0", "~ context forcer works (2)";
  is ~"1",       "1", "~ context forcer works (3)";
  is ~"0",       "0", "~ context forcer works (4)";
  is ~"",         "", "~ context forcer works (5)";
  is ~undef,      "", "~ context forcer works (6)";
  is ~"Inf",   "Inf", "~ context forcer works (7)";
  is ~"-Inf", "-Inf", "~ context forcer works (8)";
  is ~"NaN",   "NaN", "~ context forcer works (9)";
  is ~"3e5",   "3e5", "~ context forcer works (10)";
}
