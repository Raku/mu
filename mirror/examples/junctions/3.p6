my @oldval  = (5, 8, 12);
my @newval1 = (17, 15, 14);
my @newval2 = (15, 7,  20);

if (any(@newval1) < any(@oldval)) {
  say "at least 1 newval1 is less than oldvals";
}

if (any(@newval2) < any(@oldval)) {
  say "at least 1 newval2 is less than oldvals";
}


