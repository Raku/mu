#!/usr/bin/pugs

use v6;
use Test;

plan 10;

=pod

Conflict resolution role tests, see L<S12/"Conflict Resolution">

=cut

# L<S12/"Conflict Resolution">
my ($was_in_sentry_shake, $was_in_pet_shake, $was_in_general_shake) = (0) xx 3;
eval_ok '
  role Sentry { method shake() { $was_in_sentry_shake++; "A" } }
  role Pet    { method shake() { $was_in_pet_skake++;    "B" } }

  class General {
    does Sentry;
    does Pet;

    method shake(Str $what) {
      $was_in_general_shake++;
      given $what {
        when "sentry" { return ./Sentry::shake() }
        when "pet"    { return ./Pet::shake()    }
      }
    }
  }
', "role and class definition worked", :todo<feature>;

my $a;
eval_ok '$a = General.new()',      "basic class instantiation works", :todo<feature>;
eval_is '$a.shake("sentry")', "A", "conflict resolution works (1-1)", :todo<feature>;
is      $was_in_general_shake,  1, "conflict resolution works (1-2)", :todo<feature>;
is      $was_in_sentry_shake,   1, "conflict resolution works (1-3)", :todo<feature>;
# As usual, is instead of todo_is to avoid unexpected suceedings.
is      $was_in_pet_shake,      0, "conflict resolution works (1-4)";
eval_is '$a.shake("pet")',    "B", "conflict resolution works (2-1)", :todo<feature>;
is      $was_in_general_shake,  2, "conflict resolution works (2-2)", :todo<feature>;
is      $was_in_sentry_shake,   1, "conflict resolution works (2-3)", :todo<feature>;
is      $was_in_pet_shake,      1, "conflict resolution works (2-4)", :todo<feature>;
