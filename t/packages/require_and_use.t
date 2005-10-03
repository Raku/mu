#!/usr/bin/pugs

use v6;
use Test;

plan 18;
force_todo 1..2, 4..12;

if $?PUGS_BACKEND ne "BACKEND_PUGS" {
  skip_rest "PIL2JS and PIL-Run do not support eval() yet.";
  exit;
}

my @tests = (
  "t::packages::RequireAndUse1", { $^a == 42 },
  "t::packages::RequireAndUse2", { $^a != 23 },
  "t::packages::RequireAndUse3", { $^a != 23 },
);

for @tests -> $mod, $expected_ret {
  state $i = 1;

  my @strings = (
    "use ::$mod",
    "require '{ $mod.split("::").join("/") ~ ".pm" }'",
  );

  for @strings -> $str {
    diag $str;
    my $retval = try { eval $str };

    ok defined($retval) && $retval != -1 && $expected_ret($retval),
      "require or use's return value was correct ({$i++})";
    # XXX: Keys of %*INC not yet fully decided (module name? module object?),
    # IIRC.
    ok defined(%*INC{$mod}) && %*INC{$mod} != -1 && $expected_ret(%*INC{$mod}),
      "\%*INC was updated correctly ({$i++})";
  }
}

our $loaded   = 0;
our $imported = 0;

eval q{use t::packages::LoadCounter; 1} err die "error loading package: $!";
is($loaded,   1, "use loads a module");
is($imported, 1, "use calls &import");

eval q{use t::packages::LoadCounter; 1} err die "error loading package: $!";
is($loaded,   1, "a second use doesn't load the module again");
is($imported, 2, "a second use does call &import again");

eval q{no t::packages::LoadCounter; 1} err die "error no'ing package: $!";
is($loaded,   1, "&no doesn't load the module again");
is($imported, 1, "&no calls &unimport");

