my @oldval  = (5, 8, 12);

my @newval1 = (17, 15, 14); # all greater
my @newval2 = (15, 7,  20); # some less some greater
my @newval3 = (3, 1, 4);    # all less

if (all(@newval3) < all(@oldval)) {
  say "newval3 is all less";
}

if (all(@newval2) < all(@oldval)) {
  say "newval2 is all less";
}

if (all(@newval1) < all(@oldval)) {
  say "newval1 is all less";
}


