#!/usr/bin/pugs

use v6;
use Test;

my @tests = (
  "t::packages::RequireAndUse1", { $^a == 42 },
  "t::packages::RequireAndUse2", { $^a != 23 },
  "t::packages::RequireAndUse3", { $^a != 23 },
);

plan +@tests / 2 * 4;
force_todo 1..2, 4..12;

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
