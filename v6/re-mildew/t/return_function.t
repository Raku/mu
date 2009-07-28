say "1..5";
my sub foo($code) {
   $code();
   say "not ok 2 - inside foo";
   say "not ok 2 - inside foo";
}
my sub bar() {
   say "ok 1";
   foo({ return "ok 3 - value returned"; });
   say "not ok 2 - inside bar";
}
my sub baz() {
   return "ok 4 - one item", "ok 5 - other item";
}
my $a = bar();
say "ok 2 - outside bar";
say $a;
my $b = baz();
print "# baz returned ";
say $b.elems;
say $b.positional(0);
say $b.positional(1);
