sub is_even (Int $val) returns Bool { ?(($val <= 0) ?? 1 !! is_odd($val - 1))  }
sub is_odd  (Int $val) returns Bool { ?(($val <= 0) ?? 0 !! is_even($val - 1)) }

say "... mutually recursive even and odd predicates";
say is_even(4);
say is_odd(4);
say is_even(5);
say is_odd(5);

